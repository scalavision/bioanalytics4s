package vcf

enum DataType:
  // e values from −2 31 to −2 31 + 7 cannot be stored in the binary 
  // version and therefore are disallowed in both VCF and BCF
  case Integer
  // ^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$ or ^[-+]?(INF|INFINITY|NAN)$ case insensitively)
  // ∗Note Java’s Double.valueOf is particular about capitalisation, so additional code
  // is needed to parse all VCF infinite/NaN values
  case Float
  case Character
  case String

/**
 * For all of the structured lines (##INFO, ##FORMAT, ##FILTER, etc.),
 * extra fields can be included after the default fields.
 * 
 * VCF header may include tags describing the reference and contigs backing
 * the data contained in the file. These tags are based on the SQ field from the
 * SAM spec.
 */

enum NumberType:
  case A
  case R
  case G
  case `.`
  case Flag
  case Length(value: Int)

enum MetaInfo:
  // first line of the vcf metainfo (required in vcf version 4.3)
  case FileFormat(value: String)
  case FileDate(value: String)
  // Possible Types for INFO fields are: Integer, Float, Flag, Character, and String
  // In addition all values in InfoType can be used
  // The ‘Flag’ type indicates that the INFO field does not contain a Value entry,
  // and hence the Number must be 0 in this case.
  case INFO(id: String, nrOfValues: NumberType, tpe: DataType, description: String, additionalFields: IndexedSeq[(String, String)] = IndexedSeq.empty)
  case FILTER(id: String, description: String)
  case FORMAT(id: String, nrOfValues: NumberType, tpe: DataType, description: String, additionalFields: IndexedSeq[(String, String)] = IndexedSeq.empty)
  //TODO: Special defined fields for Structural Variants
  //TODO: IUPAC ambiguity codes
  case ALT(id: String, description: String)
  //TODO: encode as url
  //The URL field specifies the location of a fasta file containing breakpoint assemblies
  // referenced in the VCF records for structural variants via the BKPTID INFO key
  case Assembly(value: String)
  case Contig(id: String, length: Option[Int], additionalFields: Map[String, String])
  case META(id: String, additionalFields: Map[String, String])
  case SAMPLE(id: String, additionalFields: Map[String, String])
  case PEDIGREE(id: String, original: String)
  //TODO encode as url
  case PedigreeDB(url: String)
  case Undefined(name: String, value: String)
  case Reference(value: String)

//TODO: I am pretty sure you can calculate those lengths at compiletime
// using the scala.compiletime package
// inline val INFO_length=INFO.length() + 2
// inline val fileformat_length=fileformat.length() + 2
// inline val FILTER_length=FILTER.length() + 2
// inline val FORMAT_length=FORMAT.length() + 2

object MetaTags:
  inline val fileformat="fileformat"
  inline val fileDate="fileDate"
  inline val INFO="INFO"
  inline val FILTER="FILTER"
  inline val FORMAT="FORMAT"
  inline val ALT="ALT"
  inline val META="META"
  inline val assembly="assembly"
  inline val contig="contig"
  inline val reference="reference"
  inline val SAMPLE="SAMPLE"

enum ParsedValue:
  case IdValue(name: String, keyValues: Map[String, String])
  case SimpleValue(name: String, value: String)

object MetaInfo:

  case class KeyValue(key: String, value: String)
  case class FieldAccum(
    parseState: PState,
    keyValue: KeyValue,
    accum: IndexedSeq[(String, String)]
  )

  enum PState:
    case Key
    case Value
    case SwitchingToValue
    case SwitchingToKey

  def valueOfMetaKeyValue: String => String = _.split('=').last
  val ID: Vector[String] => String = v => valueOfMetaKeyValue(v(0))
  val Number: Vector[String] => String = v => valueOfMetaKeyValue(v(1))
  val Type: Vector[String] => String = v => valueOfMetaKeyValue(v(2))

  def toMapFromIndex(index: Int): Vector[String] => IndexedSeq[(String, String)] = v =>
    val descriptionFields = v.drop(index).mkString(",")
    val result = descriptionFields.foldLeft(FieldAccum(PState.Key, KeyValue("", ""), IndexedSeq.empty)) { (acc, c) =>
      def secondLast() = 
        acc.keyValue.value.dropRight(1).last
      def thirdLast() =
        acc.keyValue.value.dropRight(2).last
      c match
        case '"' if acc.parseState == PState.SwitchingToValue => 
          acc.copy(
            keyValue = acc.keyValue.copy(
              value = acc.keyValue.value :+ c
            ),
            parseState = PState.Value
          )

        case ',' if acc.keyValue.value.last == '"' && acc.parseState == PState.Value && secondLast() != '\\' =>
          acc.copy(parseState = PState.SwitchingToKey)

        //Spaces are hopefully not allowed in info fields, but we allow for one space to exist
        case ',' if acc.keyValue.value.last == ' ' && secondLast() == '"'  && thirdLast() != '\\' =>
          println("WARNING: Found a whitespace in between a key / value pair in the metainfo header, this could be an invalid metainfo header")
          acc.copy(parseState = PState.SwitchingToKey)

        case '=' if acc.parseState == PState.Key => acc.copy(parseState = PState.SwitchingToValue)

        case c if acc.parseState == PState.SwitchingToKey => 
          acc.copy(
            accum = acc.accum :+ (acc.keyValue.key -> acc.keyValue.value),
            keyValue = acc.keyValue.copy(
              key = c.toString(),
              value = ""
            ),
            parseState = PState.Key
          )

        case c if acc.parseState == PState.Key => 
          acc.copy(keyValue = acc.keyValue.copy(
            key = acc.keyValue.key :+ c
          ))

        case c if acc.parseState == PState.Value =>
          acc.copy(
            keyValue = acc.keyValue.copy(
              value = acc.keyValue.value :+ c
            )
          )

        case _ =>
          println("parser info on crash:")

          println(s""""
            parsed and valid fields: ${acc.accum.mkString}
            keyValue parsing now: ${acc.keyValue}
            state of the parser: ${acc.parseState}
          """)

          println(s"This part of the metainfo field does not seem to be valid: ${descriptionFields}")
          throw new Exception(s"not able to handle this character: $c")
    }

    def removeLastTag() = 
      if result.keyValue.value.last == '>' then
        result.keyValue.value.dropRight(1)
      else
        println("warning, the last character of the metainfo field was not '>', that might mean we dropped a few characters")
        result.keyValue.value

    result.accum :+ ((result.keyValue.key -> removeLastTag()))

  val toNumber: String => NumberType = {
    case "." => NumberType.`.`
    case "A" => NumberType.A
    case "R" => NumberType.R
    case "G" => NumberType.G
    case "Flag" => NumberType.Flag
    case i =>  
      NumberType.Length(i.toInt)
  }

  val toType: String => DataType = {
    case "String" => DataType.String
    case "Integer" => DataType.Integer
    case "Float" => DataType.Float
    case "Character" => DataType.Character
  }

  val metaInfo: String => INFO = s => 
    val columns = s.split(',').toVector
    val additionalFields = toMapFromIndex(3)(columns)
    INFO(ID(columns), toNumber(Number(columns)), toType(Type(columns)), additionalFields.head._1, additionalFields.tail)

  /*
  val breakMetaIdField: String => Map[String, String] = 
    _.dropWhile(_ != '<' ).drop(1).takeWhile(_ != '>').split(",").map { line =>
      val key = line.takeWhile(_ != '=')
      val value = line.drop(key.size).drop(1)
      key -> value
    }.toMap
  
  val toNumber: String => NumberType = {
    case "." => NumberType.`.`
    case "A" => NumberType.A
    case "R" => NumberType.R
    case "G" => NumberType.G
    case "Flag" => NumberType.Flag
    case i =>  NumberType.Length(i.toInt)
  }

  val toType: String => DataType = {
    case "String" => DataType.String
    case "Integer" => DataType.Integer
    case "Float" => DataType.Float
    case "Character" => DataType.Character
  }

  val sourceValue: Array[String] => Option[String] = columns =>
    if columns.length >= 10 then Some(columns(9))
    else None

  val versionValue: Array[String] => Option[String] = columns =>
    if columns.length >= 12 then Some(columns(11))
    else None

  def listToMap(values: List[String], map: Map[String, String]): Map[String, String] = values match
    case x::y::Nil => map.updated(x, y)
    case x::y::rest => listToMap(rest, map.updated(x, y))
    case Nil => Map.empty
    case _ => throw new Exception(s"ERROR in the parser, parsing metainfo: $values, (key,value) pair seems to be uneven, probably only the key is available")


  def mapValues(limit: Int): Array[String] => Map[String, String] = columns =>
    if columns.length >= limit then
      listToMap(columns.drop(limit - 1).toList, Map.empty)
    else Map.empty

  def infoMapValues: Array[String] => Map[String, String] = mapValues(13)(_)
  def formatMapValues: Array[String] => Map[String,String] = mapValues(9)(_)

  val extractINFO: Array[String] => INFO = fields => {
    INFO(
      id = fields(1), 
      nrOfValues  = toNumber(fields(3)),
      tpe = toType(fields(5)),
      description = fields(7).trim(),
      sourceValue(fields),
      versionValue(fields),
      infoMapValues(fields)
    )
  }

  val extractFORMAT: Array[String] => FORMAT = fields => {
    FORMAT(
      id = fields(1), 
      nrOfValues = toNumber(fields(3)),
      tpe = toType(fields(5)),
      description = fields(7).trim(),
      formatMapValues(fields)
    )
  }

  val extractFILTER: Array[String] => FILTER = fields => {
    FILTER(
      id = fields(1),
      description = fields(3)
    )
  }

  val extractALT: Array[String] => ALT = fields => {
    println(fields(0))
    ALT (
      id = fields(1),
      description = fields(3)
    )
  }

  val extractMETA: Array[KeyValue] => META = fields => {
    META (
      id = fields(1).value,
      mapValues(3)(fields)
    )
  }

  val extractSAMPLE: Array[String] => SAMPLE = fields => {
    SAMPLE (
      id = fields(1),
      mapValues(3)(fields)
    )
  }

  val extractContig: Array[String] => Contig = fields => {
    val result: (Option[Int], Map[String, String]) =
      if fields.length >= 3 then
        val map = mapValues(5)(fields)
        try
          val length = fields(3).toInt
          (Some(length), map)
        catch
          case _ => (None, map)
      else
        (None, Map.empty)

    //println(result)

    Contig (
      id = fields(1),
      result._1,
      result._2
    )
  }
*/


  def apply(line: String): MetaInfo =
    val metaType = line.drop(2).takeWhile(_ != '=')
    val dataLine = line.dropWhile(_ != '=').drop(1)
    val columnsByComma = line.drop(1).split(',')
    metaType match
      case MetaTags.INFO => metaInfo(dataLine.dropWhile(_ != '<').drop(1))//extractINFO(breakMetaIdField(line.drop(MetaTags.INFO.length + 1)))
      // case MetaTags.FORMAT => extractFORMAT(breakMetaIdField(line.drop(MetaTags.FORMAT.length + 1)))
      // case MetaTags.ALT => extractALT(breakMetaIdField(line.drop(MetaTags.ALT.length + 1)))
      // case MetaTags.META => 
      //   println("META line:")
      //   println(line)
      //   val idLine = breakMetaIdField(line.drop(MetaTags.META.length + 1))
      //   println(s"id: ${idLine}")
      //   extractMETA(idLine)
      // case MetaTags.SAMPLE => extractSAMPLE(breakMetaIdField(line.drop(MetaTags.SAMPLE.length + 1)))
      // case MetaTags.fileformat => FileFormat(line.drop(MetaTags.fileformat.size + 3).trim())
      // case MetaTags.fileDate => FileDate(line.drop(MetaTags.fileDate.size + 3))
      // case MetaTags.contig => extractContig(breakMetaIdField(line.drop(MetaTags.contig.length + 1)))
      // case MetaTags.reference => Reference(line.drop(MetaTags.reference.size + 3).trim())
      //   // extractContig(breakMetaIdField(line.drop(MetaTags.reference.length + 1)))
      // case meta if line.drop(metaType.length()+3).headOption.getOrElse('c') == '<' =>
      //   val idLine = breakMetaIdField(line.drop(MetaTags.META.length + 1))
      //   println(s"id: ${idLine}")
      //   println(s"here it is: $meta")
      //   extractMETA(idLine)
      case _ => throw new Exception(s"Undefined metainfo: $line, with token: $metaType, for value check: ${line.drop(metaType.length()+1).head}")
    /*

      def iterateMetaLine(
    line: List[Char],
    index: Int,
    tmp: (String, String),
    accum: Map[String, String]
  ): Map[String, String] =
    line match
      case Nil => accum
      case '>' :: Nil => accum
      case '<' :: xs => 
        iterateMetaLine(xs, index, tmp, accum)
      case '=' :: xs if index == 0 => iterateMetaLine(xs, 1, ("",""), Map("fieldType" -> tmp._1))
      case '=' :: xs if index == 1 => iterateMetaLine(xs, 2, tmp, accum)
      case x :: xs if index == 0 => iterateMetaLine(xs, 0, (tmp._1 + x.toString(), ""), accum)
      case x :: xs if index == 1 => iterateMetaLine(xs, 1, (tmp._1 + x, ""), accum)
      case '"' :: xs if index == 2 => iterateMetaLine(xs, 3, tmp, accum)
      case x :: xs if index == 2 => iterateMetaLine(xs, 2, (tmp._1, tmp._2 + x), accum)
      case x :: xs if index == 3 => iterateMetaLine(xs, 4, tmp, accum)
      case '\\' :: xs if index == 4 => iterateMetaLine(xs, 4, (tmp._1, tmp._2 + """\""""), accum)
      case x :: xs if index == 4 => iterateMetaLine(xs, 4, (tmp._1, tmp._2 + x), accum)
      case '"' :: xs if index == 4 => iterateMetaLine(xs, 5, tmp, accum)
      case ',' :: xs if index == 5 => iterateMetaLine(xs, 0, ("", ""), accum ++ Map(tmp._1 -> tmp._2))
      case _ => throw new Exception(s"unbalancd: ${line.mkString}, $tmp, $accum")

  def metaLineToParsedValue: String => ParsedValue = line =>
    val metaType = line.drop(2).takeWhile(_ != '=')
    val dropLength = metaType.length + 3
    val data = line.drop(dropLength)
    val tagged = data.take(1)
    val next = data.drop(1)
    tagged match 
      case "<" => 
        assert(next.last == '>', s"meta line $line does not end with '>', but  ${next.last}")
        val keyValues = next.dropRight(1).split(',').map { keyValue =>
          val key = keyValue.takeWhile(_ != '=')
          val value = keyValue.drop(key.size).drop(1)
          key -> value
        }.toMap
        ParsedValue.IdValue(metaType, keyValues)
      case _ =>
        ParsedValue.SimpleValue(metaType, next)


    val metaType = line.drop(2).takeWhile(_ != '=')



    metaType match
      case MetaTags.INFO => extractINFO(breakMetaIdField(line.drop(MetaTags.INFO.length + 1)))
      case MetaTags.FORMAT => extractFORMAT(breakMetaIdField(line.drop(MetaTags.FORMAT.length + 1)))
      case MetaTags.ALT => extractALT(breakMetaIdField(line.drop(MetaTags.ALT.length + 1)))
      case MetaTags.META => 
        println("META line:")
        println(line)
        val idLine = breakMetaIdField(line.drop(MetaTags.META.length + 1))
        println(s"id: ${idLine}")
        extractMETA(idLine)
      case MetaTags.SAMPLE => extractSAMPLE(breakMetaIdField(line.drop(MetaTags.SAMPLE.length + 1)))
      case MetaTags.fileformat => FileFormat(line.drop(MetaTags.fileformat.size + 3).trim())
      case MetaTags.fileDate => FileDate(line.drop(MetaTags.fileDate.size + 3))
      case MetaTags.contig => extractContig(breakMetaIdField(line.drop(MetaTags.contig.length + 1)))
      case MetaTags.reference => Reference(line.drop(MetaTags.reference.size + 3).trim())
        // extractContig(breakMetaIdField(line.drop(MetaTags.reference.length + 1)))
      case meta if line.drop(metaType.length()+3).headOption.getOrElse('c') == '<' =>
        val idLine = breakMetaIdField(line.drop(MetaTags.META.length + 1))
        println(s"id: ${idLine}")
        println(s"here it is: $meta")
        extractMETA(idLine)
      case _ => throw new Exception(s"Undefined metainfo: $line, with token: $metaType, for value check: ${line.drop(metaType.length()+1).head}")
    */