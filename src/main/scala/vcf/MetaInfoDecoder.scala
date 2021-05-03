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
  case META(id: String, additionalFields: IndexedSeq[(String, String)])
  case SAMPLE(id: String, additionalFields: IndexedSeq[(String, String)])
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
      if(result.keyValue.value.isEmpty) ""
      else if (result.keyValue.value.last == '>') 
          result.keyValue.value.dropRight(1)
      else {
        println("warning, the last character of the metainfo field was not '>', that might mean we dropped a few characters")
        result.keyValue.value
      }

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

  val metaInfo: Vector[String] => INFO = columns => 
    val additionalFields = toMapFromIndex(3)(columns)
    INFO(ID(columns), toNumber(Number(columns)), toType(Type(columns)), additionalFields.head._2, additionalFields.tail)

  val metaFormat: Vector[String] => FORMAT = columns =>
    val additionalFields = toMapFromIndex(3)(columns)
    FORMAT(ID(columns), toNumber(Number(columns)), toType(Type(columns)), additionalFields.head._2, additionalFields.tail)
  
  val breakMetaIdField: String => Map[String, String] = 
    _.dropWhile(_ != '<' ).drop(1).takeWhile(_ != '>').split(',').map { line =>
      val key = line.takeWhile(_ != '=')
      val value = line.drop(key.size).drop(1)
      key -> value
    }.toMap

  val columnToMap: Vector[String] => Map[String, String] = _.map { line =>
      val key = line.takeWhile(_ != '=')
      val value = line.drop(key.size).drop(1).filter(_.isDigit)
      key -> value
    }.toMap

  val extractContig: Vector[String] => Contig = fields => {
    val mappedFields = columnToMap(fields)
    Contig (
      id = mappedFields("ID"),
      length = mappedFields.get("length").fold(None)(s => Some(s.toInt)),
      (mappedFields - "ID") - "length"
    )
  }

  val extractFORMAT: Vector[String] => FORMAT = columns => {
    val additionalFields = toMapFromIndex(3)(columns)
    FORMAT(ID(columns),toNumber(Number(columns)), toType(Type(columns)), additionalFields.head._2, additionalFields.tail)
  }

  val extractFILTER: Vector[String] => FILTER = fields => {
    FILTER(
      id = fields(1),
      description = fields(3)
    )
  }

  val extractALT: Vector[String] => ALT = fields => {
    ALT (
      id = fields(1),
      description = fields(3)
    )
  }

  val extractMETA: Vector[String] => META = columns => {
    META (
      id = columns(1),
      toMapFromIndex(3)(columns)
    )
  }

  val extractSAMPLE: Vector[String] => SAMPLE = columns => {
    SAMPLE (
      id = columns(1),
      toMapFromIndex(3)(columns)
    )
  }

  def apply(line: String): MetaInfo =

    val metaType = line.drop(2).takeWhile(_ != '=')
    val dataLine = line.dropWhile(_ != '=').drop(1)
    val columnsByComma = line.drop(1).split(',')
    val columns = dataLine.dropWhile(_ != '<').drop(1).split(',').toVector
    
    metaType match
      case MetaTags.INFO => metaInfo(columns)
      case MetaTags.FORMAT => metaFormat(columns)
      case MetaTags.contig => extractContig(columns)
      case MetaTags.ALT => extractALT(columns)
      case MetaTags.SAMPLE => extractSAMPLE(columns)
      case MetaTags.fileformat => FileFormat(line.drop(MetaTags.fileformat.size + 3).trim())
      case MetaTags.fileDate => FileDate(line.drop(MetaTags.fileDate.size + 3))
      case MetaTags.reference => Reference(line.drop(MetaTags.reference.size + 3).trim())
      case meta if line.drop(metaType.length()+3).headOption.getOrElse('c') == '<' =>
        extractMETA(columns)
      case _ => throw new Exception(s"Undefined metainfo: $line, with token: $metaType, for value check: ${line.drop(metaType.length()+1).head}")
