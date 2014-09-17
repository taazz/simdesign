{ unit NativeXmlEx

  This is a small-footprint implementation to read and write XML documents
  natively from Delpi code. NativeXmlEx has very fast parsing speeds.

  You can use this code to read XML documents from files, streams or strings.
  The load routine generates events that can be used to display load progress
  on the fly.

  Note: any external encoding (ANSI, UTF16, etc) is converted to an internal
  encoding that is UTF8. NativeXml uses UTF8String as string type internally,
  and converts from strings with external encoding in the parsing process.
  When writing, UTF8String strings are converted to the external encoding strings,
  if the encoding was set beforehand, or defaults to UTF8 if no encoding was set.

  External     -    Internal    -     API
  --------          --------          ---
  In file:                           ....Ansi functions - only for legacy 
  "encoding=XXXX"     UTF8           ....UTF8 functions
                                     ....Wide functions

       Parse              UTF8ToAnsi, UTF8ToWide
       ----->             --------------------->

        Write             AnsiToUTF8, WideToUTF8
       <-----             <---------------------


  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 - 2010 Simdesign B.V. (www.simdesign.nl)
  Contributor(s): 

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit NativeXmlEx;

interface

{$i NativeXml.inc}

uses
  Windows, Classes, Contnrs, SysUtils, NativeXmlParser, NativeXmlWriter,
  NativeXmlUtils, sdStringTable;

const

  // Current version of the NativeXml unit
  cNativeXmlExVersion = 'v3.20';

type

  // Note on TNativeXmlEx.Format:
  // - xfReadable to be able to read the xml file with a standard editor.
  // - xfCompact (default) to save the xml fully compliant and at smallest size
  TsdXmlFormatType = (
    xfCompact,  // Save without any control chars except LF after declarations
    xfReadable, // Save in readable format with indents and CRLF
    xfLinux     // Save in readable format with indents and LF
  );

  // An event that is used to indicate load or save progress.
  TXmlProgressEvent = procedure(Sender: TObject; Size: integer) of object;

  // Forward declarations
  TCustomXmlEx = class;
  TsdAttribute = class;

  // TXmlNode metaclass
  TsdNodeClass = class of TXmlNode;

  // TXmlNode is the ancestor for all nodes in the xml document. See TsdElement
  // for the elements, TsdAttribute for the attributes.
  TXmlNode = class(TPersistent)
  private
    FOwner: TCustomXmlEx;
    FParent: TXmlNode;
    {$IFDEF USETAGS}
    FTag: integer;
    {$ENDIF}
    {$IFDEF SOURCEPOS}
    FSourcePos: int64;
    {$ENDIF}
    function GetAttributeByName(const AName: Utf8String): TsdAttribute;
    function GetAttributeValueByName(const AName: Utf8String): Utf8String;
    procedure SetAttributeValueByName(const AName, Value: Utf8String);
    function GetValueWide: WideString;
    procedure SetValueWide(const Value: WideString);
    function GetAttributes(Index: integer): TsdAttribute;
    function GetAttributeName(Index: integer): Utf8String;
    function GetAttributeValue(Index: integer): Utf8String;
    procedure SetAttributeName(Index: integer; const Value: Utf8String);
    procedure SetAttributeValue(Index: integer; const Value: Utf8String);
    function GetAttributeValueAsInteger(Index: integer): integer;
    procedure SetAttributeValueAsInteger(Index: integer; const Value: integer);
    function GetWriteOnDefault: boolean;
    procedure SetWriteOnDefault(const Value: boolean);
  protected
    // string table lookup methods
    function TableGetString(AID: integer): Utf8String;
    procedure TableSetString(var AID: integer; const S: Utf8String);
    function GetName: Utf8String; virtual;
    function GetValue: Utf8String; virtual;
    procedure SetName(const Value: Utf8String); virtual;
    procedure SetValue(const Value: Utf8String); virtual;
    function GetNodes(Index: integer): TXmlNode; virtual;
    class function EscapeString(const S: Utf8String): Utf8String;
    class function UnescapeString(const S: Utf8String): Utf8String;
    procedure ParseStream(P: TsdBufferParser); virtual;
    procedure WriteStream(S: TStream); virtual;
    function GetIndent: Utf8String; virtual;
    function GetLineFeed: Utf8String; virtual;
    function NodeFindOrCreate(const AName: Utf8String): TXmlNode; virtual;
    procedure WriteValue(const AName, AValue: Utf8String); virtual;
    property WriteOnDefault: boolean read GetWriteOnDefault write SetWriteOnDefault;
  public
    // Create a new node object. AOwner must be the TsdFastXml that is
    // going to hold this new node. Make sure to use the correct class when
    // creating, e.g. TsdElement.Create(Owner) for an element.
    constructor Create(AOwner: TCustomXmlEx); virtual;
    // Convert the UTF8 string S to a WideString
    class function UTF8ToWide(const S: Utf8String): WideString;
    // Convert the WideString W to an UTF8 string
    class function WideToUTF8(const W: WideString): Utf8String;
    // The element type of this node.
    function ElementType: TsdElementType; virtual;
    {$IFDEF USETAGS}
    // Tag is an integer value the developer can use in any way. Tag does not get
    // saved to the XML. Tag is often used to point to a GUI element (and is then
    // cast to a pointer).
    property Tag: integer read FTag write FTag;
    {$ENDIF}
    {$IFDEF SOURCEPOS}
    // SourcePos (int64) points to the position in the source file where the
    // nodes text begins.
    property SourcePos: int64 read FSourcePos write FSourcePos;
    {$ENDIF}
    // Parent points to the parent node of the current XML node.
    property Parent: TXmlNode read FParent;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node Name and value are empty.
    function IsClear: boolean;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node value is empty.
    function IsEmpty: boolean;
    // Delete this node. It will be removed from its parent list and removed
    // from memory completely.
    procedure Delete;
    // Get the number of attributes in this node
    function AttributeCount: integer; virtual;
    // Use this method to add an attribute with name AName and string value AValue
    // to the node. AName and AValue must be UTF8 encoded.
    procedure AttributeAdd(const AName, AValue: Utf8String);
    // Use this method to delete the attribute at Index in the list. Index must be
    // equal or greater than 0, and smaller than AttributeCount. Using an index
    // outside of that range has no effect.
    procedure AttributeDelete(Index: integer);
    // Add the node ANode to the nodelist. It will be added at the end, unless
    // it is an attribute, in that case it will be added at the end of the current
    // list of attributes. NodeAdd will set the parent of ANode to itself.
    function NodeAdd(ANode: TXmlNode): integer; virtual;
    // Return a reference to the first subnode in the nodelist that has name AName.
    // If no subnodes with AName are found, the function returns nil.
    function NodeByName(const AName: Utf8String): TXmlNode;
    // Number of subnodes present in this node (this includes attributes,
    // cdata, char-data, sub-elements, etcetera).
    function NodeCount: integer; virtual;
    //
    procedure NodesAdd(Nodes: array of TXmlNode);
    // Use this procedure to retrieve all nodes that have name AName. Pointers to
    // these nodes are added to the list in AList. AList must be initialized
    // before calling this procedure. If you use a TXmlNodeList you don't need
    // to cast the list items to TXmlNode.
    procedure NodesByName(const AName: Utf8String; const AList: TList);
    // \Delete the subnode at Index. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeDelete(Index: integer); virtual;
    // Call NodeIndexOf to get the index for ANode in the Nodes list. The first
    // node in the list has index 0, the second item has index 1, and so on. If
    // a node is not in the list, NodeIndexOf returns -1.
    function NodeIndexOf(ANode: TXmlNode): integer; virtual;
    // Insert the node ANode at location Index in the list. Make sure to honour
    // the fact that attributes are also nodes, and should always be first in
    // the list. You can find the number of attributes with AttributeCount.
    procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
    // Switch position of the nodes at Index1 and Index2.
    procedure NodeExchange(Index1, Index2: integer); virtual;
    // \Create a new node with AName, add it to the subnode list, and return a
    // pointer to it.
    function NodeNew(const AName: Utf8String): TXmlNode; virtual;
    // Return the first subnode with AType, or nil if none
    function FirstNodeByType(AType: TsdElementType): TXmlNode; virtual;
    // Read TreeDepth to find out many nested levels there are for the current XML
    // node. Root has a TreeDepth of zero.
    function TreeDepth: integer;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property Name: Utf8String read GetName write SetName;
    // The value of the node. For elements this is the element value (based on
    // first chardata fragment), for attributes this is the attribute value. The
    // string is encoded as UTF8. Use ToWide(Node.Value) or Node.ValueWide
    // to get a WideString compatible with "wide" windows methods.
    property Value: Utf8String read GetValue write SetValue;
    // ValueWide returns the value of the node as a WideString.
    property ValueWide: WideString read GetValueWide write SetValueWide;
    // List of attributes present in this element. Use AttributeCount to iterate.
    property Attributes[Index: integer]: TsdAttribute read GetAttributes;
    // Get or set the name of the attribute at Index (as UTF8).
    property AttributeName[Index: integer]: Utf8String read GetAttributeName write SetAttributeName;
    // Get or set the value of the attribute at Index (as UTF8).
    property AttributeValue[Index: integer]: Utf8String read GetAttributeValue write SetAttributeValue;
    // Read this property to get the integer value of the attribute at index Index.
    // If the value cannot be converted, 0 will be returned. Write to it to set the
    // integer value.
    property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
    // Get a reference to an attribute node by its name. If there is no attribute
    // with that name, nil will be returned.
    property AttributeByName[const AName: Utf8String]: TsdAttribute read GetAttributeByName;
    // Get the value of an attribute with name AName. If no attribute is present,
    // an empty string is returned. When setting this value, an attribute is
    // created if it does not yet exist.
    property AttributeValueByName[const AName: Utf8String]: Utf8String read
      GetAttributeValueByName write SetAttributeValueByName;
    // List of subnodes, by index. Iterate through the list using NodeCount
    // and this property. The attributes are listed first, then followed by
    // all other node types, in the order as found in the XML document.
    property Nodes[Index: integer]: TXmlNode read GetNodes; default;

    // ValueAsXYZ functions

    // Convert the node's value to boolean and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsBoolDef(ADefault: boolean): boolean; virtual;
    // Convert the node's value to a TDateTime and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsDateTimeDef(ADefault: TDateTime): TDateTime; virtual;
    // Convert the node's value to integer and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsIntegerDef(ADefault: integer): integer; virtual;

    // ReadXYZ functions

    // Read the subnode with AName and convert it to a boolean value. If the
    // subnode is not found, or cannot be converted, the boolean ADefault will
    // be returned.
    function ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean; virtual;
    // Read the subnode with AName and convert its value to TDateTime. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadDateTime(const AName: Utf8String; ADefault: TDateTime = 0): TDateTime; virtual;
    // Read the subnode with AName and convert its value to an integer. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadInteger(const AName: Utf8String; ADefault: integer = 0): integer; virtual;
    // Read the subnode with AName and return its UTF8String value. If the subnode is
    // not found, ADefault will be returned.
    function ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String; virtual;

    // WriteXYZ functions

    // Add or replace the subnode with AName and set its value to represent the boolean
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteBool(const AName: Utf8String; AValue: boolean; ADefault: boolean = False); virtual;
    // Add or replace the subnode with AName and set its value to represent the TDateTime
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    // The XML format used is compliant with W3C's specification of date and time.
    procedure WriteDateTime(const AName: Utf8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the integer
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInteger(const AName: Utf8String; AValue: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the UTF8String
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteString(const AName, AValue: Utf8String; const ADefault: Utf8String = ''); virtual;
  end;

  // List of nodes
  TsdNodeList = class(TObjectList)
  private
    function GetItems(Index: integer): TXmlNode;
  public
    // ByType returns the first item in the list that has element type AType.
    // If no item is found, the function returns nil.
    function ByType(AType: TsdElementType): TXmlNode;
    property Items[Index: integer]: TXmlNode read GetItems; default;
  end;

  // Node representing a xml char-data fragment
  TsdCharData = class(TXmlNode)
  private
    FValueID: integer;
  protected
    function MustEscape: boolean; virtual;
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing quoted text ('bla' or "bla")
  TsdQuotedText = class(TsdCharData)
  private
    FQuoteChar: AnsiChar;
  protected
    function GetName: Utf8String; override;
    function MustEscape: boolean; override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    constructor Create(AOwner: TCustomXmlEx); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml attribute.
  TsdAttribute = class(TsdQuotedText)
  private
    FNameID: integer;
  protected
    function MustEscape: boolean; override;
    function GetName: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
  protected
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // TsdContainerNode is the base class for all element types that can have
  // sub-nodes.
  TsdContainerNode = class(TXmlNode)
  private
    FNodes: TsdNodeList;
  protected
    function ParseAttributeList(P: TsdBufferParser): AnsiChar; virtual;
    function ParseQuotedTextList(P: TsdBufferParser): AnsiChar; virtual;
    function WriteAttributeList(S: TStream): integer; virtual;
    function GetNodes(Index: integer): TXmlNode; override;
    property NodeList: TsdNodeList read FNodes;
  public
    constructor Create(AOwner: TCustomXmlEx); override;
    function AttributeCount: integer; override;
    function NodeCount: integer; override;
    function NodeAdd(ANode: TXmlNode): integer; override;
    procedure NodeDelete(Index: integer); override;
    function NodeIndexOf(ANode: TXmlNode): integer; override;
    procedure NodeInsert(Index: integer; ANode: TXmlNode); override;
    procedure NodeExchange(Index1, Index2: integer); override;
    function FirstNodeByType(AType: TsdElementType): TXmlNode; override;
    destructor Destroy; override;
  end;

  // Node representing an xml element.
  TsdElement = class(TsdContainerNode)
  private
    FNameID: integer;
  protected
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure SetValue(const Value: Utf8String); override;
    procedure ParseIntermediateData(R: TsdBufferParser); virtual;
    procedure ParseElementList(P: TsdBufferParser; const SupportedTags: TsdElementTypes); virtual;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml declaration, e.g. <?xml version="1.0"?>
  TsdDeclaration = class(TsdContainerNode)
  private
    function GetEncoding: Utf8String;
    function GetVersion: Utf8String;
    procedure SetEncoding(const Value: Utf8String);
    procedure SetVersion(const Value: Utf8String);
  protected
    function GetName: Utf8String; override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
    property Version: Utf8String read GetVersion write SetVersion;
    property Encoding: Utf8String read GetEncoding write SetEncoding;
  end;

  // Node representing an xml comment. Get/set Value for the comment.
  TsdComment = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // Node representing a CData element. Get/Set value for the data in CDATA.
  TsdCData = class(TsdComment)
  protected
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // DocType declaration element. It can have sub-nodes with dtd elements,
  // entities, notations, etc.
  TsdDocType = class(TsdElement)
  private
    FExternalId: Utf8String;
    FSystemLiteral: TsdQuotedText;
    FPubIDLiteral: TsdQuotedText;
  protected
    procedure ParseIntermediateData(P: TsdBufferParser); override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    constructor Create(AOwner: TCustomXmlEx); override;
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
    // External ID: either SYSTEM or PUBLIC
    property ExternalId: Utf8String read FExternalId write FExternalId;
    // The system literal without quotes
    property SystemLiteral: TsdQuotedText read FSystemLiteral;
    // The PubID literal without quotes
    property PubIDLiteral: TsdQuotedText read FPubIDLiteral;
  end;

  // DTD Element declaration
  TsdDtdElement = class(TsdElement)
  protected
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD AttList declaration
  TsdDtdAttList = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Entity declaration
  TsdDtdEntity = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Notation declaration
  TsdDtdNotation = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  TsdInstruction = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
    procedure ParseStream(P: TsdBufferParser); override;
  public
    function ElementType: TsdElementType; override;
  end;

  TsdStyleSheet = class(TsdInstruction)
  protected
    function GetName: Utf8String; override;
    procedure ParseStream(P: TsdBufferParser); override;
    procedure WriteStream(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // todo - what is the exclamation mark?
  TsdExclam = class(TXmlNode);

  TsdXmlNodeEvent = procedure(Sender: TObject; ANode: TXmlNode) of object;

  // TCustomXmlEx is a very fast XML reader (15 Mb per second). Use Create to
  // create a new instance, use LoadFromFile/LoadFromStream to load the XML
  // document from a file or stream.
  TCustomXmlEx = class(TComponent)
  private
    FExternalEncoding: TsdStringEncoding;
    FExternalCodePage: integer;
    FExternalBomInfo: TBomInfo;
    FRootNodes: TsdNodeList;
    FParser: TsdBufferParser;
    FXmlFormat: TsdXmlFormatType;
    FAbortParsing: boolean;
    FParserWarnings: boolean;
    FPreserveWhitespace: boolean;
    FWriteOnDefault: boolean;
    FOnNodeNew: TsdXmlNodeEvent;
    FOnNodeLoaded: TsdXmlNodeEvent;
    FOnProgress: TXmlProgressEvent;
    FIndentString: Utf8String; // Called after a node is loaded/saved, with the current position in the file
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);
    function GetReaderPosition: int64;
    function GetCommentString: Utf8String;
    procedure SetCommentString(const Value: Utf8String);
    function GetStyleSheet: TsdStyleSheet;
    function GetEncodingString: Utf8String;
    procedure SetEncodingString(const Value: Utf8String);
    function GetRoot: TsdElement;
    function GetVersionString: Utf8String;
    procedure SetVersionString(const Value: Utf8String);
    function GetReaderLineNumber: int64;
  protected
    FStringTable: TsdStringTable;
    procedure DoProgress(Size: integer);
    function LineFeed: Utf8String;
    procedure ParseStream(P: TsdBufferParser);
    procedure WriteStream(S: TStream);
  public
    // Create a new TNativeXml component.
    constructor Create(AOwner: TComponent); override;
    // Use CreateName to Create a new Xml document that will automatically
    // contain a root element with name ARootName.
    constructor CreateName(const ARootName: Utf8String; AOwner: TComponent = nil); virtual;
    destructor Destroy; override;
    // Clear all the nodes in the xml document
    procedure Clear; virtual;
    // IndentString is the string used for indentations. By default, it is two
    // spaces: '  '. Set IndentString to something else if you need to have
    // specific indentation, or set it to an empty string to avoid indentation.
    property IndentString: Utf8String read FIndentString write FIndentString;
    // Function IsEmpty returns true if the root is clear, or in other words, the
    // root contains no value, no name, no subnodes and no attributes.
    function IsEmpty: boolean;
    // Call procedure LoadFromFile to load an XML document from the filename
    // specified. See Create for an example. The LoadFromFile procedure will raise
    // an exception when it encounters non-wellformed XML.
    procedure LoadFromFile(const AFileName: string); virtual;
    // Load an XML document from the TStream object in AStream. The LoadFromStream
    // procedure will raise an exception when it encounters non-wellformed XML.
    // This method can be used with any TStream descendant. The stream is read
    // from chunk-wise (using 64K chunks). See also LoadFromFile and ReadFromString.
    procedure LoadFromStream(AStream: TStream); virtual;
    // Call procedure ReadFromString to load an XML document from the UTF8String AValue.
    // The ReadFromString procedure will raise an exception of type EFilerError
    // when it encounters non-wellformed XML.
    procedure ReadFromString(const AValue: Utf8String); virtual;
    // Call SaveToFile to save the XML document to a file with FileName. If the
    // filename exists, it will be overwritten without warning. If the file cannot
    // be created, a standard I/O exception will be generated. Set XmlFormat to
    // xfReadable if you want the file to contain indentations to make the XML
    // more human-readable. This is not the default and also not compliant with
    // the XML specification.
    procedure SaveToFile(const AFileName: string); virtual;
    // Call SaveToStream to save the XML document to the Stream. Stream
    // can be any TStream descendant. Set XmlFormat to xfReadable if you want
    // the stream to contain indentations to make the XML more human-readable. This
    // is not the default and also not compliant with the XML specification. See
    // SaveToFile for information on how to save in special encoding.
    procedure SaveToStream(Stream: TStream); virtual;
    // Call WriteToStringUTF8 to write the XML document to an UTF8 string.
    function WriteToString: Utf8String; virtual;
    // Root is the topmost element in the XML document. Access Root to read any
    // child elements. When creating a new XML document, you can automatically
    // include a Root element, by creating using CreateName.
    property Root: TsdElement read GetRoot;
    // RootNodes can be used to directly access the nodes in the root of the
    // XML document. Usually this list consists of one declaration node followed
    // by an element node which is the Root. You can use this property to add or
    // delete comments, stylesheets, dtd's etc.
    property RootNodes: TsdNodeList read FRootNodes;
    // A comment string above the root element \<!--{comment}--\> can be accessed with
    // this property. \Assign a comment to this property to add it to the XML document.
    // Use property RootNodeList to add/insert/extract multiple comments.
    property CommentString: Utf8String read GetCommentString write SetCommentString;
    // After reading, this property contains the XML version (usually "1.0").
    property VersionString: Utf8String read GetVersionString write SetVersionString;
    // Encoding string (e.g. "UTF-8" or "UTF-16"). This encoding string is stored in
    // the header.
    // Example: In order to get this header:
    // <?xml version="1.0" encoding="UTF-16" ?>
    // enter this code:
    // <CODE>MyXmlDocument.EncodingString := 'UTF-16';</CODE>
    // When reading a file, EncodingString will contain the encoding used.
    property EncodingString: Utf8String read GetEncodingString write SetEncodingString;
    // StringTable holds all the content (strings) in the xml tree
    property StringTable: TsdStringTable read FStringTable;
    // Get the stylesheet used for this XML document. If the node does not
    // exist yet, it will be created (thus if you use this property, and don't
    // set any of the attributes, an empty stylesheet node will be the result).
    property StyleSheet: TsdStyleSheet read GetStyleSheet;
    // External encoding is valid after loading, and indicates the encoding
    // detected in the file/stream. Internally, all string values are always
    // encoded in UTF8, so if the external stream is Ascii or UTF16, a conversion
    // is done. When writing to a file/stream, a BOM is generated for the encoding
    // and a conversion is done from UTF8 to this encoding if necessary.
    property ExternalEncoding: TsdStringEncoding read FExternalEncoding write FExternalEncoding;
    // XmlFormat by default is set to xfCompact. This setting is compliant to the spec,
    // and NativeXml will only generate XML files with #$0A as control character.
    // By setting OutputFormatting to xfReadable, you can generate readable XML
    // files that contain indentation and CR after each element.
    property XmlFormat: TsdXmlFormatType read FXmlFormat write FXmlFormat;
    property OnProgress: TXmlProgressEvent read FOnProgress write FOnProgress;
    // Set PreserveWhiteSpace to True to preserve all whitespace present in the
    // file when reading. The blocks of whitespace are stored as CharData nodes.
    property PreserveWhiteSpace: boolean read FPreserveWhiteSpace write FPreserveWhiteSpace;
    // ReaderPosition gives the reader's current position in the stream when
    // loading.
    property ReaderPosition: int64 read GetReaderPosition;
    // ReaderLineNumber gives the readers current line number in the stream
    // when loading.
    property ReaderLineNumber: int64 read GetReaderLineNumber;
    // Set AbortParsing to True if you use the OnNodeNew and OnNodeLoaded events in
    // a SAX-like manner, and you want to abort the parsing process halfway. Example:
    // <code>
    // procedure MyForm.NativeXmlNodeLoaded(Sender: TObject; Node: TXmlNode);
    // begin
    //   if (Node.Name = 'LastNode') and (Sender is TNativeXml) then
    //     TNativeXml(Sender).AbortParsing := True;
    // end;
    // </code>
    property AbortParsing: boolean read FAbortParsing write FAbortParsing;
    // Set WriteOnDefault to False if you do not want to write default values to
    // the XML document. This option can avoid creating huge documents with
    // redundant info, and will speed up writing.
    property WriteOnDefault: boolean read FWriteOnDefault write FWriteOnDefault;
    // Connect to OnNodeNew to get informed of new nodes being added while loading.
    property OnNodeNew: TsdXmlNodeEvent read FOnNodeNew write FOnNodeNew;
    // Connect to OnNodeLoaded to get informed of nodes being finished loading.
    property OnNodeLoaded: TsdXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
  end;

  // TNativeXmlEx adds more methods to the core TCustomXmlEx
  TNativeXmlEx = class(TCustomXmlEx)
  public
    function NodeNew(const AName: Utf8String): TXmlNode; overload; virtual;
    function NodeNew(const AName: Utf8String; SubNodes: array of TXmlNode): TXmlNode; overload; virtual;
    function NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;
    //{$REGION 'string nodes'}
    function NodeNewText(AName, AValue: Utf8String): TXmlNode; overload;

{    function NodeNewTextEx(lName, lValue: string; out pXMLNode: TXmlNode): TXmlNode; overload;
    function NodeNewText(lName, lValue: string; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextEx(lName, lValue: string; out pXMLNode: TXmlNode;
      lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextType(lName, lValue: string;
      lElementType: TXmlElementType): TXmlNode; overload;
    function NodeNewTextTypeEx(lName, lValue: string;
      lElementType: TXmlElementType; out pXMLNode: TXmlNode): TXmlNode; overload;
    function NodeNewTextType(lName, lValue: string;
      lElementType: TXmlElementType; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeEx(lName, lValue: string; lElementType: TXmlElementType;
      out pXMLNode: TXmlNode; lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextAttr(lName, lValue: string; lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewTextAttrEx(lName, lValue: string; out pXMLNode: TXmlNode;
      lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewTextAttr(lName, lValue: string; lAttributes: array of TAttribute;
      lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextAttrEx(lName, lValue: string; out pXMLNode: TXmlNode;
      lAttributes: array of TAttribute; lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewTextTypeAttr(lName, lValue: string; lElementType: TXmlElementType;
      lAttributes: array of TAttribute): TXmlNode; overload;}
    function NodeNewTextTypeAttr(AName, AValue: Utf8String; AElementType: TsdElementType;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType;
      out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;



{    function NodeNewTextTypeAttr(lName, lValue: string; lElementType: TXmlElementType;
      lAttributes: array of TAttribute; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(lName, lValue: string; lElementType: TXmlElementType;
      out pXMLNode: TXmlNode; lAttributes: array of TAttribute;
      lSubNodes: array of TXmlNode): TXmlNode; overload;}
    //{$ENDREGION}

    //{$REGION 'integer nodes'}
    function NodeNewInt(AName: Utf8String; AValue: integer): TXmlNode; overload;
{    function NodeNewIntEx(lName: string; lValue: integer; out pXMLNode: TXmlNode): TXmlNode; overload;
    function NodeNewInt(lName: string; lValue: integer; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntEx(lName: string; lValue: integer; out pXMLNode: TXmlNode;
      lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntType(lName: string; lValue: integer;
      lElementType: TXmlElementType): TXmlNode; overload;
    function NodeNewIntTypeEx(lName: string; lValue: integer;
      lElementType: TXmlElementType; out pXMLNode: TXmlNode): TXmlNode; overload;
    function NodeNewIntType(lName: string; lValue: integer;
      lElementType: TXmlElementType; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeEx(lName: string; lValue: integer; lElementType: TXmlElementType;
      out pXMLNode: TXmlNode; lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntAttr(lName: string; lValue: integer; lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewIntAttrEx(lName: string; lValue: integer; out pXMLNode: TXmlNode;
      lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewIntAttr(lName: string; lValue: integer; lAttributes: array of TAttribute;
      lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntAttrEx(lName: string; lValue: integer; out pXMLNode: TXmlNode;
      lAttributes: array of TAttribute; lSubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntTypeAttr(lName: string; lValue: integer; lElementType: TXmlElementType;
      lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewIntTypeAttrEx(lName: string; lValue: integer; lElementType: TXmlElementType;
      out pXMLNode: TXmlNode; lAttributes: array of TAttribute): TXmlNode; overload;
    function NodeNewIntTypeAttr(lName: string; lValue: integer; lElementType: TXmlElementType;
      lAttributes: array of TAttribute; lSubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeAttrEx(lName: string; lValue: integer; lElementType: TXmlElementType;
      out pXMLNode: TXmlNode; lAttributes: array of TAttribute;
      lSubNodes: array of TXmlNode): TXmlNode; overload;}
    //{$ENDREGION}
  end;

var

  // XML Defaults

  cDefaultEncodingString:          Utf8String          = 'UTF-8';
  cDefaultExternalEncoding:        TsdStringEncoding   = seUTF8;
  cDefaultVersionString:           Utf8String          = '1.0';
  cDefaultXmlFormat:               TsdXmlFormatType     = xfCompact;
  cDefaultWriteOnDefault:          boolean             = True;
  cDefaultIndentString:            Utf8String          = '  ';
  cDefaultDropCommentsOnParse:     boolean             = False;
  cDefaultUseFullNodes:            boolean             = False;
  cDefaultSortAttributes:          boolean             = False;
  cDefaultFloatAllowScientific:    boolean             = True;
  cDefaultFloatSignificantDigits:  integer             = 6;

const

  cElementTypeNames: array[TsdElementType] of Utf8String =
    ('Element', 'Attribute', 'Comment', 'CData', 'Declaration', 'Stylesheet',
     'DocType', 'DtdElement', 'DtdAttList', 'DtdEntity', 'DtdNotation',
     'Question', 'CharData', 'QuotedText', 'Unknown', 'EndTag', 'Error');


implementation

const

  cNodeClass: array[TsdElementType] of TsdNodeClass =
    (TsdElement, TsdAttribute, TsdComment, TsdCData, TsdDeclaration, TsdStyleSheet,
     TsdDocType, TsdDtdElement, TsdDtdAttList, TsdDtdEntity, TsdDtdNotation,
     TsdInstruction, TsdCharData, TsdQuotedText, nil, nil, nil);

{ TXmlNode }

procedure TXmlNode.AttributeAdd(const AName, AValue: Utf8String);
var
  A: TsdAttribute;
begin
  A := TsdAttribute.Create(FOwner);
  A.Name := AName;
  A.Value := AValue;
  NodeAdd(A);
end;

function TXmlNode.AttributeCount: integer;
begin
  Result := 0;
end;

procedure TXmlNode.AttributeDelete(Index: integer);
begin
  if Index < AttributeCount then
    NodeDelete(Index);
end;

constructor TXmlNode.Create(AOwner: TCustomXmlEx);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TXmlNode.Delete;
begin
  // Free by removing from owning list:
  // Remove from our parent
  if FParent is TsdContainerNode then
    TsdContainerNode(FParent).NodeList.Remove(Self)
  else
    // Remove from owner root list.
    FOwner.FRootNodes.Remove(Self);
end;

function TXmlNode.ElementType: TsdElementType;
begin
  Result := xeUnknown;
end;

class function TXmlNode.EscapeString(const S: Utf8String): Utf8String;
begin
  Result := sdEscapeString(S);
end;

function TXmlNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := nil;
end;

class function TXmlNode.WideToUTF8(const W: WideString): Utf8String;
begin
  Result := sdWideToUtf8(W);
end;

function TXmlNode.GetAttributeByName(const AName: Utf8String): TsdAttribute;
var
  i: integer;
begin
  for i := 0 to AttributeCount - 1 do
    if (Nodes[i].Name = AName) then
    begin
      Result := TsdAttribute(Nodes[i]);
      exit;
    end;
  Result := nil;
end;

function TXmlNode.GetAttributeName(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if assigned(A) then
    Result := A.Name
  else
    Result := '';
end;

function TXmlNode.GetAttributes(Index: integer): TsdAttribute;
var
  Node: TXmlNode;
begin
  Node := GetNodes(Index);
  if Node is TsdAttribute then
    Result := TsdAttribute(Node)
  else
    Result := nil;
end;

function TXmlNode.GetAttributeValue(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
  Result := StrToIntDef(GetAttributeValue(Index), 0);
end;

function TXmlNode.GetAttributeValueByName(const AName: Utf8String): Utf8String;
var
  A: TsdAttribute;
begin
  A := AttributeByName[AName];
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetIndent: Utf8String;
var
  i: integer;
begin
  if assigned(FOwner) then
  begin
    case FOwner.XmlFormat of
    xfCompact: Result := '';
    xfReadable:
      for i := 0 to TreeDepth - 1 do
        Result := Result + FOwner.IndentString;
    end; //case
  end else
    Result := ''
end;

function TXmlNode.GetLineFeed: Utf8String;
begin
  if assigned(FOwner) then
  begin
    case FOwner.XmlFormat of
    xfCompact:  Result := '';
    xfReadable: Result := #13#10;
    else
      Result := #10;
    end; //case
  end else
    Result := '';
end;

function TXmlNode.GetName: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := nil;
end;

function TXmlNode.GetValue: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetValueWide: WideString;
begin
  Result := sdUtf8ToWide(GetValue);
end;

function TXmlNode.IsClear: boolean;
begin
  Result := IsEmpty and (length(Name) = 0);
end;

function TXmlNode.IsEmpty: boolean;
begin
  Result := (NodeCount = 0) and (length(Value) = 0)
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeByName(const AName: Utf8String): TXmlNode;
var
  i: integer;
begin
  for i := 0 to NodeCount - 1 do
    if Nodes[i].Name = AName then
    begin
      Result := Nodes[i];
      exit;
    end;
  Result := nil;
end;

function TXmlNode.NodeCount: integer;
begin
  Result := 0;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
  raise Exception.Create(sCannotManipulate);
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeNew(const AName: Utf8String): TXmlNode;
// Add a new child node and return its pointer
begin
  // based on element type
  case ElementType of
  xeElement: Result := TsdElement.Create(FOwner);
  xeDocType: Result := TsdDocType.Create(FOwner);
  xeDtdElement: Result := TsdDtdElement.Create(FOwner);
  xeDtdAttlist: Result := TsdDtdAttlist.Create(FOwner);
  xeDtdEntity: Result := TsdDtdEntity.Create(FOwner);
  xeDtdNotation: Result := TsdDtdNotation.Create(FOwner);
  xeDeclaration: Result := TsdDeclaration.Create(FOwner);
  else
    Result := nil;
  end;
  if assigned(Result) then
  begin
    Result.Name := AName;
    NodeAdd(Result);
  end;
end;

procedure TXmlNode.ParseStream(P: TsdBufferParser);
begin
// does nothing
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if not assigned(A) then
    exit;
  A.Name := Value;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if not assigned(A) then exit;
  A.Value := Value;
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
  SetAttributeValue(Index, IntToStr(Value));
end;

procedure TXmlNode.SetAttributeValueByName(const AName, Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if not assigned(A) then
  begin
    A := TsdAttribute.Create(FOwner);
    A.Name := AName;
    NodeAdd(A);
  end;
  A.Value := Value;
end;

procedure TXmlNode.SetName(const Value: Utf8String);
begin
  raise Exception.Create(sCannotSetName);
end;

procedure TXmlNode.SetValue(const Value: Utf8String);
begin
  raise Exception.Create(sCannotSetValue);
end;

procedure TXmlNode.SetValueWide(const Value: WideString);
begin
  SetValue(sdWideToUtf8(Value));
end;

function TXmlNode.TableGetString(AID: integer): Utf8String;
begin
  Result := FOwner.FStringTable.GetString(AID);
end;

procedure TXmlNode.TableSetString(var AID: integer; const S: Utf8String);
begin
  FOwner.FStringTable.SetString(AID, S);
end;

class function TXmlNode.UTF8ToWide(const S: Utf8String): WideString;
begin
  Result := sdUtf8ToWide(S);
end;

function TXmlNode.TreeDepth: integer;
begin
  if assigned(FParent) then
    Result := FParent.TreeDepth + 1
  else
    Result := 0;
end;

class function TXmlNode.UnescapeString(const S: Utf8String): Utf8String;
begin
  Result := sdUnescapeString(S);
end;

procedure TXmlNode.WriteStream(S: TStream);
begin
  // most functionality is in descendants
  // Call the onprogress
  if assigned(FOwner) then
    FOwner.DoProgress(S.Position);
end;

function TXmlNode.ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean;
var
  N: TXmlNode;
begin
  Result := ADefault;
  N := NodeByName(AName);
  if assigned(N) then
    Result := N.ValueAsBoolDef(ADefault);
end;

function TXmlNode.ReadDateTime(const AName: Utf8String;
  ADefault: TDateTime): TDateTime;
var
  N: TXmlNode;
begin
  Result := ADefault;
  N := NodeByName(AName);
  if assigned(N) then
    Result := N.ValueAsDateTimeDef(ADefault);
end;

function TXmlNode.ReadInteger(const AName: Utf8String; ADefault: integer): integer;
var
  N: TXmlNode;
begin
  Result := ADefault;
  N := NodeByName(AName);
  if assigned(N) then
    Result := N.ValueAsIntegerDef(ADefault);
end;

function TXmlNode.ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String;
var
  N: TXmlNode;
begin
  Result := ADefault;
  N := NodeByName(AName);
  if assigned(N) then
    Result := N.Value;
end;

function TXmlNode.ValueAsBoolDef(ADefault: boolean): boolean;
begin
  Result := StrToBoolDef(Value, ADefault);
end;

function TXmlNode.ValueAsDateTimeDef(ADefault: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(Value, ADefault);
end;

function TXmlNode.ValueAsIntegerDef(ADefault: integer): integer;
begin
  Result := StrToIntDef(Value, ADefault);
end;

procedure TXmlNode.NodesByName(const AName: Utf8String; const AList: TList);
// Fill AList with nodes that have name AName
var
  i: integer;
begin
  if not assigned(AList) then
    exit;
  AList.Clear;
  for i := 0 to NodeCount - 1 do
    if UTF8CompareText(Nodes[i].Name, AName) = 0 then
      AList.Add(Nodes[i]);
end;

procedure TXmlNode.WriteBool(const AName: Utf8String; AValue, ADefault: boolean);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdBoolToString(AValue));
end;

procedure TXmlNode.WriteDateTime(const AName: Utf8String; AValue, ADefault: TDateTime);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdDateTimeToString(AValue));
end;

procedure TXmlNode.WriteInteger(const AName: Utf8String; AValue, ADefault: integer);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdIntToString(AValue));
end;

procedure TXmlNode.WriteString(const AName, AValue, ADefault: Utf8String);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, AValue);
end;

procedure TXmlNode.NodesAdd(Nodes: array of TXmlNode);
var
  x: integer;
begin
  for x := Low(Nodes) to High(Nodes) do
    NodeAdd(Nodes[x]);
end;

function TXmlNode.GetWriteOnDefault: boolean;
begin
  if assigned(FOwner) then
    Result := FOwner.WriteOnDefault
  else
    Result := False;
end;

procedure TXmlNode.SetWriteOnDefault(const Value: boolean);
begin
  if assigned(FOwner) then
    FOwner.WriteOnDefault := Value;
end;

function TXmlNode.NodeFindOrCreate(const AName: Utf8String): TXmlNode;
// Find the node with AName, and if not found, add new one
begin
  Result := NodeByName(AName);
  if not assigned(Result) then
    Result := NodeNew(AName);
end;

procedure TXmlNode.WriteValue(const AName, AValue: Utf8String);
var
  N: TXmlNode;
begin
  N := NodeFindOrCreate(AName);
  if assigned(N) then
    N.Value := AValue;
end;

{ TsdCharData }

destructor TsdCharData.Destroy;
begin
  TableSetString(FValueID, '');
  inherited;
end;

function TsdCharData.ElementType: TsdElementType;
begin
  Result := xeCharData;
end;

function TsdCharData.GetName: Utf8String;
begin
  Result := 'CharData';
end;

function TsdCharData.GetValue: Utf8String;
begin
  if MustEscape then
    Result := UnescapeString(TableGetString(FValueID))
  else
    Result := TableGetString(FValueID);
end;

function TsdCharData.MustEscape: boolean;
begin
  Result := True;
end;

procedure TsdCharData.SetValue(const Value: Utf8String);
begin
  if MustEscape then
    TableSetString(FValueID, EscapeString(Value))
  else
    TableSetString(FValueID, Value);
end;

{ TsdAttribute }

destructor TsdAttribute.Destroy;
begin
  TableSetString(FNameID, '');
  inherited;
end;

function TsdAttribute.ElementType: TsdElementType;
begin
  Result := xeAttribute;
end;

function TsdAttribute.GetName: Utf8String;
begin
  Result := TableGetString(FNameID);
end;

function TsdAttribute.MustEscape: boolean;
begin
  Result := True;
end;

procedure TsdAttribute.ParseStream(P: TsdBufferParser);
begin
  {$IFDEF SOURCEPOS}
  FSourcePos := P.Position;
  {$ENDIF}
  // Get the attribute name
  TableSetString(FNameID, Trim(P.ReadStringUntilChar('=', False)));
  // value
  inherited;
end;

procedure TsdAttribute.SetName(const Value: Utf8String);
begin
  TableSetString(FNameID, Value);
end;

procedure TsdAttribute.WriteStream(S: TStream);
begin
  sdWriteToStream(S, Name + '=');
  inherited;
end;

{ TsdQuotedText }

constructor TsdQuotedText.Create(AOwner: TCustomXmlEx);
begin
  inherited;
  FQuoteChar := '"';
end;

function TsdQuotedText.ElementType: TsdElementType;
begin
  Result := xeQuotedText;
end;

function TsdQuotedText.GetName: Utf8String;
begin
  Result := 'QuotedText';
end;

function TsdQuotedText.MustEscape: boolean;
begin
  Result := False;
end;

procedure TsdQuotedText.ParseStream(P: TsdBufferParser);
begin
  // Get the quoted value
  FQuoteChar := P.NextCharSkipBlanks;
  if not (FQuoteChar in ['''', '"']) then
    raise Exception.CreateFmt(sQuoteCharExpected, [P.Position]);
  TableSetString(FValueID, P.ReadQuotedString(FQuoteChar));
end;

procedure TsdQuotedText.WriteStream(S: TStream);
begin
  sdWriteToStream(S, FQuoteChar + Value + FQuoteChar);
  inherited;
end;

{ TsdContainerNode }

function TsdContainerNode.AttributeCount: integer;
begin
  Result := 0;
  while FNodes[Result] is TsdAttribute do
    inc(Result);
end;

constructor TsdContainerNode.Create(AOwner: TCustomXmlEx);
begin
  inherited;
  FNodes := TsdNodeList.Create(True);
end;

destructor TsdContainerNode.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

function TsdContainerNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := FNodes.ByType(AType);
end;

function TsdContainerNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := FNodes[Index];
end;

function TsdContainerNode.NodeAdd(ANode: TXmlNode): integer;
begin
  if ANode.ElementType = xeAttribute then
  begin
    Result := AttributeCount;
    FNodes.Insert(Result, ANode);
  end else
    Result := FNodes.Add(ANode);
  ANode.FParent := Self;
end;

function TsdContainerNode.NodeCount: integer;
begin
  Result := FNodes.Count;
end;

procedure TsdContainerNode.NodeDelete(Index: integer);
begin
  FNodes.Delete(Index);
end;

procedure TsdContainerNode.NodeExchange(Index1, Index2: integer);
begin
  FNodes.Exchange(Index1, Index2);
end;

function TsdContainerNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := FNodes.IndexOf(ANode);
end;

procedure TsdContainerNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  FNodes.Insert(Index, ANode);
  ANode.FParent := Self;
end;

function TsdContainerNode.ParseAttributeList(P: TsdBufferParser): AnsiChar;
var
  A: TsdAttribute;
begin
  repeat
    Result := P.NextCharSkipBlanks;
    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;
    P.MoveBack;
    A := TsdAttribute.Create(FOwner);
    NodeAdd(A);
    A.ParseStream(P);
    FOwner.DoNodeNew(A);
    FOwner.DoNodeLoaded(A);
  until False;
end;

function TsdContainerNode.ParseQuotedTextList(P: TsdBufferParser): AnsiChar;
var
  T: TsdQuotedText;
begin
  repeat
    Result := P.NextCharSkipBlanks;
    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;
    P.MoveBack;
    T := TsdQuotedText.Create(FOwner);
    T.ParseStream(P);
    NodeAdd(T);
    FOwner.DoNodeNew(T);
    FOwner.DoNodeLoaded(T);
  until False;
end;

function TsdContainerNode.WriteAttributeList(S: TStream): integer;
var
  i: integer;
  SN: TXmlNode;
begin
  Result := 0;
  for i := 0 to NodeCount  - 1 do
  begin
    SN := FNodes[i];
    // write attributes
    if SN is TsdAttribute then
    begin
      sdWriteToStream(S, ' ');
      SN.WriteStream(S);
      inc(Result);
    end;
  end;
end;

{ TsdElement }

destructor TsdElement.Destroy;
begin
  TableSetString(FNameID, '');
  inherited;
end;

function TsdElement.ElementType: TsdElementType;
begin
  Result := xeElement;
end;

function TsdElement.GetName: Utf8String;
begin
  Result := TableGetString(FNameID);
end;

function TsdElement.GetValue: Utf8String;
var
  Node: TXmlNode;
begin
  // Return the value of the first subnode after the attributes if it is CharData
  Node := FNodes[AttributeCount];
  if Node is TsdCharData then
    Result := Node.Value
  else
    Result := '';
end;

procedure TsdElement.ParseElementList(P: TsdBufferParser; const SupportedTags: TsdElementTypes);
var
  B: AnsiChar;
  N: Utf8String;
  ET: TsdElementType;
  NC: TsdNodeClass;
  Node: TXmlNode;
begin
  repeat
    // Process char data
    ParseIntermediateData(P);
    // Process subtags and end tag
    if P.EndOfStream then
      raise Exception.CreateFmt(sPrematureEnd, [P.Position]);
    P.MoveBack;
    B := P.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ET := P.ReadOpenTag;
      if not (ET in SupportedTags) then
        raise Exception.CreateFmt(sIllegalTag, [P.Position]);
      // End tag?
      if ET = xeEndTag then
      begin
        // Read end tag
        N := P.ReadStringUntilChar('>', False);
        // Check if begin and end tag match
        if TableGetString(FNameID) <> N then
          raise Exception.CreateFmt(sBeginEndMismatch, [TableGetString(FNameID), N, P.Position]);
        // We're done reading this element, so we will set the capacity of the
        // nodelist to just the amount of items to avoid having overhead.
        FNodes.SetCapacity(FNodes.Count);
        exit;
      end;
      // Determine node class
      NC := cNodeClass[ET];
      // for now..
      if not assigned(NC) then
        raise Exception.CreateFmt(sUnsupportedTag, [P.Position]);
      // Create new node and add
      Node := NC.Create(FOwner);
      NodeAdd(Node);
      if ET <> xeElement then
        FOwner.DoNodeNew(Node);
      // The node will parse itself
      Node.ParseStream(P);
      FOwner.DoNodeLoaded(Node);
    end else
    begin
      // Since this virtual proc is also used for doctype parsing.. check
      // end char here
      if (B = ']') and (ElementType = xeDocType) then
        break;
    end;
  until FOwner.FAbortParsing;
end;

procedure TsdElement.ParseIntermediateData(R: TsdBufferParser);
var
  ChData: Utf8String;
  CD: TsdCharData;
  SP: int64;
begin
  {$IFDEF SOURCEPOS}
  SP := R.Position;
  {$ENDIF}
  ChData := R.ReadStringUntilChar('<', True);
  if not FOwner.PreserveWhiteSpace then
    ChData := Trim(ChData);

  if length(ChData) > 0 then
  begin
    // Insert CharData node
    CD := TsdCharData.Create(FOwner);
    {$IFDEF SOURCEPOS}
    CD.FSourcePos := SP;
    {$ENDIF}
    TablesetString(CD.FValueID, ChData);
    NodeAdd(CD);
    FOwner.DoNodeNew(CD);
    FOwner.DoNodeLoaded(CD);
  end;
end;

procedure TsdElement.ParseStream(P: TsdBufferParser);
var
  B: AnsiChar;
  SP: int64;
begin
  // Flush the reader.. it will only flush when it has read a large chunk of data,
  // or when encoding/codepage is determined so this routine is not costly.
  P.Flush;

  // Parse name
  {$IFDEF SOURCEPOS}
  SP := P.Position;
  {$ENDIF}
  TableSetString(FNameID, Trim(P.ReadStringUntilBlankOrEndTag));
  {$IFDEF SOURCEPOS}
  FSourcePos := SP;
  {$ENDIF}
  FOwner.DoNodeNew(Self);

  // Parse attribute list
  B := ParseAttributeList(P);
  if B = '/' then
  begin
    // Direct tag
    B := P.NextChar;
    if B <> '>' then
      raise Exception.CreateFmt(sIllegalEndTag, [P.Position]);
  end else
  begin
    if B <> '>' then
      raise Exception.CreateFmt(sIllegalEndTag, [P.Position]);
    ParseElementList(P, [xeElement..xeCData, xeInstruction..xeEndTag]);
  end;
end;

procedure TsdElement.SetName(const Value: Utf8String);
begin
  TableSetString(FNameID, Value);
end;

procedure TsdElement.SetValue(const Value: Utf8String);
var
  Count: integer;
  Node: TXmlNode;
begin
  // Return the value of the first subnode after the attributes if it is CharData
  Count := AttributeCount;
  Node := FNodes[Count];
  if Node is TsdCharData then
    Node.Value := Value
  else
  begin
    Node := TsdCharData.Create(FOwner);
    Node.Value := Value;
    NodeInsert(Count, Node);
  end;
end;

procedure TsdElement.WriteStream(S: TStream);
var
  i, AttrCount: integer;
  SN: TXmlNode;
begin
  // write element
  sdStreamWrite(S, GetIndent + '<' + Name);

  // write attributes
  AttrCount := WriteAttributeList(S);

  if (NodeCount = AttrCount) and (Value = '') then
  begin

    // direct node
    sdStreamWrite(S, ' />');

  end else
  begin
    // value
    sdStreamWrite(S, '>' + Value + GetLineFeed);

    // sub-elements
    for i := AttrCount to NodeCount - 1 do
    begin
      SN := FNodes[i];
      if SN is TsdElement then
      begin
        SN.WriteStream(S);
      end;
    end;

    // endtag
    sdStreamWrite(S, GetIndent + '</' + Name + '>');

  end;

  sdStreamWrite(S, GetLineFeed);
  inherited;
end;

{ TsdDeclaration }

function TsdDeclaration.ElementType: TsdElementType;
begin
  Result := xeDeclaration;
end;

function TsdDeclaration.GetEncoding: Utf8String;
begin
  Result := AttributeValueByName['encoding'];
end;

function TsdDeclaration.GetName: Utf8String;
begin
  Result := 'xml';
end;

function TsdDeclaration.GetVersion: Utf8String;
begin
  Result := AttributeValueByName['version'];
end;

procedure TsdDeclaration.ParseStream(P: TsdBufferParser);
var
  B: AnsiChar;
begin
  // Directly parse the attribute list
  B := ParseAttributeList(P);
  if B <> '?' then
    raise Exception.CreateFmt(sIllegalEndTag, [P.Position]);
  B := P.NextChar;
  if B <> '>' then
    raise Exception.CreateFmt(sIllegalEndTag, [P.Position]);
end;

procedure TsdDeclaration.SetEncoding(const Value: Utf8String);
begin
  AttributeValueByName['encoding'] := Value;
end;

procedure TsdDeclaration.SetVersion(const Value: Utf8String);
begin
  AttributeValueByName['version'] := Value;
end;

procedure TsdDeclaration.WriteStream(S: TStream);
begin
  // XML declaration <?xml{declaration}?>
  // Explicitly delete empty attributes in the declaration,
  // this is usually the encoding and we do not want encoding=""
  // to show up
//todo
{  DeleteEmptyAttributes;}
  sdWriteToStream(S, GetIndent + '<?xml');
  WriteAttributeList(S);
  sdWriteToStream(S, '?>' + GetLineFeed);
  inherited;
end;

{ TsdComment }

function TsdComment.ElementType: TsdElementType;
begin
  Result := xeComment;
end;

function TsdComment.GetName: Utf8String;
begin
  Result := 'Comment';
end;

procedure TsdComment.ParseStream(P: TsdBufferParser);
begin
  TableSetString(FValueID, P.ReadStringUntil('-->'));
end;

procedure TsdComment.WriteStream(S: TStream);
begin
  // Comment <!--{comment}-->
  sdWriteToStream(S, GetIndent + '<!--' + Value + '-->');
  inherited;
end;

{ TsdCData }

function TsdCData.ElementType: TsdElementType;
begin
  Result := xeCData;
end;

function TsdCData.GetName: Utf8String;
begin
  Result := 'CData';
end;

function TsdCData.GetValue: Utf8String;
begin
  // The value should not be unescaped
  Result := TableGetString(FValueID);
end;

procedure TsdCData.ParseStream(P: TsdBufferParser);
begin
  TableSetString(FValueID, P.ReadStringUntil(']]>'));
end;

procedure TsdCData.SetValue(const Value: Utf8String);
begin
  // The value should not be escaped
  TableSetString(FValueID, Value);
end;

procedure TsdCData.WriteStream(S: TStream);
var
  Line: Utf8String;
begin
  // literal data <![CDATA[{data}]]>
  Line := GetIndent + '<![CDATA[' + Value + ']]>';
  sdWriteToStream(S, Line);
  inherited;
end;

{ TsdDocType }

constructor TsdDocType.Create(AOwner: TCustomXmlEx);
begin
  inherited;
  FSystemLiteral := TsdQuotedText.Create(AOwner);
  FPubIDLiteral := TsdQuotedText.Create(AOwner);
end;

destructor TsdDocType.Destroy;
begin
  FreeAndNil(FSystemLiteral);
  FreeAndNil(FPubIDLiteral);
  inherited;
end;

function TsdDocType.ElementType: TsdElementType;
begin
  Result := xeDocType;
end;

procedure TsdDocType.ParseIntermediateData(P: TsdBufferParser);
// in dtd's we do not allow chardata, but pe instead. Not implemented yet
var
  B: AnsiChar;
begin
  repeat
    B := P.NextCharSkipBlanks;
    // todo: PERef
    if not (B in [']', '<']) then
      P.ReadStringUntilBlankOrEndTag
    else
      break;
  until False;
end;

procedure TsdDocType.ParseStream(P: TsdBufferParser);
var
  B: AnsiChar;
begin
  // Parse name
  P.SkipBlanks;
  TableSetString(FNameID, Trim(P.ReadStringUntilBlankOrEndTag));
  P.SkipBlanks;
  B := P.NextChar;
  if not (B in ['[', '>']) then
  begin
    P.MoveBack;
    // Parse external ID
    if P.CheckString('SYSTEM') then
    begin
      FExternalId := 'SYSTEM';
      FSystemLiteral.ParseStream(P);
    end else
      if P.CheckString('PUBLIC') then
      begin
        FExternalID := 'PUBLIC';
        FPubIDLiteral.ParseStream(P);
        FSystemLiteral.ParseStream(P);
      end else
        raise Exception.CreateFmt(sIllegalTag, [P.Position]);
    B := P.NextCharSkipBlanks;
  end;
  if B = '[' then
  begin
    ParseElementList(P, [xeComment, xeDtdElement..xeInstruction, xeCharData]);
    B := P.NextCharSkipBlanks;
  end;
  if B <> '>' then
    raise Exception.CreateFmt(sIllegalTag, [P.Position]);
end;

procedure TsdDocType.WriteStream(S: TStream);
begin
// todo
{  if NodeCount = 0 then
    Line := Indent + '<!DOCTYPE ' + Name + ' ' + ValueDirect + '>'
  else
  begin
    Line := Indent + '<!DOCTYPE ' + Name + ' ' + ValueDirect + ' [' + LFeed;
    sdUTF8WriteStringToStream(S, Line);
    for i := 0 to NodeCount - 1 do
    begin
      Nodes[i].WriteToStream(S);
      sdUTF8WriteStringToStream(S, LFeed);
    end;
    Line := ']>';
  end;}
  inherited;
end;

{ TsdDtdElement }

function TsdDtdElement.ElementType: TsdElementType;
begin
  Result := xeDtdElement;
end;

procedure TsdDtdElement.ParseStream(P: TsdBufferParser);
var
  B: AnsiChar;
begin
  P.SkipBlanks;
  TableSetString(FNameID, Trim(P.ReadStringUntilBlankOrEndTag));
  P.SkipBlanks;
  // For now..
  B := ParseQuotedTextList(P);
  if B <> '>' then
    raise Exception.CreateFmt(sIllegalEndTag, [P.Position]);
end;

procedure TsdDtdElement.WriteStream(S: TStream);
var
  Line, ET: Utf8String;
begin
  case ElementType of
  xeDtdElement:  ET := 'ELEMENT';
  xeDtdAttList:  ET := 'ATTLIST';
  xeDtdEntity:   ET := 'ENTITY';
  xeDtdNotation: ET := 'NOTATION';
  else
    raise EFilerError.Create(sUnsupportedTag);
  end; //case
  Line := GetIndent + Format('<!%s ', [ET]);  // + Name + ' ' + {ValueDirect} + '>';
  sdStreamWrite(S, Line);
  inherited;
end;

{ TsdDtdAttList }

function TsdDtdAttList.ElementType: TsdElementType;
begin
  Result := xeDtdAttList;
end;

{ TsdDtdEntity }

function TsdDtdEntity.ElementType: TsdElementType;
begin
  Result := xeDtdEntity;
end;

{ TsdDtdNotation }

function TsdDtdNotation.ElementType: TsdElementType;
begin
  Result := xeDtdNotation;
end;

{ TsdInstruction }

function TsdInstruction.ElementType: TsdElementType;
begin
  Result := xeInstruction;
end;

function TsdInstruction.GetName: Utf8String;
begin
  Result := 'PI';
end;

procedure TsdInstruction.ParseStream(P: TsdBufferParser);
begin
  TableSetString(FValueID, P.ReadStringUntil('?>'));
end;

{ TsdStyleSheet }

function TsdStyleSheet.ElementType: TsdElementType;
begin
  Result := xeStyleSheet;
end;

function TsdStyleSheet.GetName: Utf8String;
begin
  Result := 'xml-stylesheet';
end;

procedure TsdStyleSheet.ParseStream(P: TsdBufferParser);
begin
  TableSetString(FValueID, Trim(P.ReadStringUntil('?>')));
end;

procedure TsdStyleSheet.WriteStream(S: TStream);
var
  Line: Utf8String;
begin
  // Stylesheet <?xml-stylesheet{stylesheet}?>
// todo
{  Line := Indent + '<?xml-stylesheet' + WriteInnerTag + '?>';}
  sdWriteToStream(S, Line);
  inherited;
end;

{ TsdNodeList }

function TsdNodeList.ByType(AType: TsdElementType): TXmlNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ElementType = AType then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TsdNodeList.GetItems(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TNativeXmlEx }

procedure TCustomXmlEx.Clear;
var
  D: TsdDeclaration;
  E: TsdElement;
begin
  FStringTable.Clear;
  FRootNodes.Clear;
  // Defaults
  // todo
  // Build default items in RootNodes: first the declaration
  D := TsdDeclaration.Create(Self);
  D.Version := cDefaultVersionString;
  D.Encoding := cDefaultEncodingString;
  FRootNodes.Add(D);
  // then the root node
  E := TsdElement.Create(Self);
  FRootNodes.Add(E);
end;

constructor TCustomXmlEx.Create(AOwner: TComponent);
begin
  inherited;
  FIndentString := '  ';
  FRootNodes := TsdNodeList.Create(True);
  FStringTable := TsdStringTable.Create;
  Clear; // this sets defaults and adds default elements
end;

constructor TCustomXmlEx.CreateName(const ARootName: Utf8String; AOwner: TComponent);
begin
  Create(AOwner);
  Root.Name := ARootName;
end;

destructor TCustomXmlEx.Destroy;
begin
  FStringTable.Clear;
  FreeAndNil(FRootNodes);
  FreeAndNil(FParser);
  FreeAndNil(FStringTable);
  inherited;
end;

procedure TCustomXmlEx.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOnNodeLoaded) then
    FOnNodeLoaded(Self, ANode);
end;

procedure TCustomXmlEx.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOnNodeNew) then
    FOnNodeNew(Self, ANode);
end;

procedure TCustomXmlEx.DoProgress(Size: integer);
begin
  if assigned(FOnProgress) then
    FOnProgress(Self, Size);
end;

function TCustomXmlEx.GetCommentString: Utf8String;
// Get the first comment node, and return its value
var
  Node: TXmlNode;
begin
  Result := '';
  Node := FRootNodes.ByType(xeComment);
  if assigned(Node) then
    Result := Node.Value;
end;

function TCustomXmlEx.GetEncodingString: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Encoding;
end;

function TCustomXmlEx.GetReaderLineNumber: int64;
begin
  if assigned(FParser) then
    Result := FParser.LineNumber
  else
    Result := 0;
end;

function TCustomXmlEx.GetReaderPosition: int64;
begin
  if assigned(FParser) then
    Result := FParser.Position
  else
    Result := 0;
end;

function TCustomXmlEx.GetRoot: TsdElement;
begin
  Result := TsdElement(FRootNodes.ByType(xeElement));
end;

function TCustomXmlEx.GetStyleSheet: TsdStyleSheet;
begin
  Result := TsdStyleSheet(FRootNodes.ByType(xeStylesheet));
  if not assigned(Result) then
  begin
    // Add a stylesheet node as second one if none present
    Result := TsdStyleSheet.Create(Self);
    FRootNodes.Insert(1, Result);
  end;
end;

function TCustomXmlEx.GetVersionString: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Version;
end;

function TCustomXmlEx.IsEmpty: boolean;
var
  R: TXmlNode;
begin
  R := GetRoot;
  Result := not assigned(R) or R.IsClear;
end;

function TCustomXmlEx.LineFeed: Utf8String;
begin
  case FXmlFormat of
  xfReadable:
    Result := #13#10;
  xfCompact:
    Result := #10;
  else
    Result := #10;
  end;//case
end;

procedure TCustomXmlEx.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TCustomXmlEx.LoadFromStream(AStream: TStream);
begin
  FRootNodes.Clear;
  FParser := TsdBufferParser.Create(AStream);
  try
    ParseStream(FParser);
    FExternalEncoding := FParser.Encoding;
    FExternalCodePage := FParser.CodePage;
    FExternalBomInfo := FParser.BomInfo;
  finally
    FParser.Flush(True);
    //FParser.ClearData;
    FreeAndNil(FParser);
  end;
end;

procedure TCustomXmlEx.ParseStream(P: TsdBufferParser);
var
  B: AnsiChar;
  ET: TsdElementType;
  NC: TsdNodeClass;
  Node: TXmlNode;
  StringData: Utf8String;
  CD: TsdCharData;
  StringPointer: int64;
begin
  FAbortParsing := False;

  // read BOM
  P.ReadBOM;

  // Read next tag
  repeat
    {$IFDEF SOURCEPOS}
    StringPointer := P.Position;
    {$ENDIF}
    StringData := P.ReadStringUntilChar('<', True);
    if not FPreserveWhiteSpace then
      StringData := Trim(StringData);

    if length(StringData) > 0 then
    begin
      // Add chardata node
      CD := TsdCharData.Create(Self);
      {$IFDEF SOURCEPOS}
      CD.FSourcePos := StringPointer;
      {$ENDIF}
      FStringTable.SetString(CD.FValueID, StringData);
      FRootNodes.Add(CD);
      DoNodeNew(CD);
      DoNodeLoaded(CD);
    end;

    // At the end of the stream? Then stop
    if P.EndOfStream then
      break;
    P.MoveBack;

    B := P.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ET := P.ReadOpenTag;
      if ET = xeError then
        raise Exception.CreateFmt(sIllegalTag, [P.Position]);

      // Determine node class
      NC := cNodeClass[ET];
      if not assigned(NC) then
        raise Exception.CreateFmt(sUnsupportedTag, [P.Position]);

      // Create new node and add
      Node := NC.Create(Self);
      FRootNodes.Add(Node);
      if ET <> xeElement then
        DoNodeNew(Node);

      // The node will parse itself
      Node.ParseStream(P);
      DoNodeLoaded(Node);

      // After adding nodes, see if we added the declaration node
      if Node.ElementType = xeDeclaration then
      begin
        // Find codepage from encoding in the declaration
        FExternalCodePage := sdEncodingToCodePage(TsdDeclaration(Node).Encoding);

        // Is this stream encoded as UTF8?
        if FExternalCodePage = CP_UTF8 then
        begin
          // and the stream as Ansi? We assume the stream is UTF8
          if P.Encoding = seAnsi then
            P.Encoding := seUTF8;
        end;{ else
        begin
          if P.Encoding = seAnsi then
            // In this case we must instruct the stream to use UTF8
            P.ConvertAnsi := True;
        end;;}
      end;
    end;

    // Check if application has aborted parsing
    // todo
  until FAbortParsing;
end;

procedure TCustomXmlEx.ReadFromString(const AValue: Utf8String);
var
  S: TStream;
begin
  S := TsdUTF8StringStream.Create(AValue);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomXmlEx.SaveToFile(const AFileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomXmlEx.SaveToStream(Stream: TStream);
var
  B: TsdBufferWriter;
begin
  // Create buffer writer, which enabes correct BOM, encoding and codepage.
  B := TsdBufferWriter.Create(Stream);
  try
    // Set External encoding
    B.Encoding := FExternalEncoding;
    B.CodePage := FExternalCodePage;
    WriteStream(B);
  finally
    B.Free;
  end;
end;

procedure TCustomXmlEx.SetCommentString(const Value: Utf8String);
// Find first comment node and set it's value, otherwise add new comment node
// right below the xml declaration
var
  Node: TXmlNode;
begin
  Node := FRootNodes.ByType(xeComment);
  if not assigned(Node) and (length(Value) > 0) then
  begin
    Node := TsdComment.Create(Self);
    FRootNodes.Insert(1, Node);
  end;
  if assigned(Node) then
    Node.Value := Value;
end;

procedure TCustomXmlEx.SetEncodingString(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if Value = GetEncodingString then
    exit;
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Encoding := Value;
end;

procedure TCustomXmlEx.SetVersionString(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if Value = GetVersionString then
    exit;
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Version := Value;
end;

procedure TCustomXmlEx.WriteStream(S: TStream);
var
  i: integer;
begin
  if not assigned(Root) and FParserWarnings then
    raise EFilerError.Create(sRootElementNotDefined);

  DoProgress(0);

  // write the root nodes
  for i := 0 to FRootNodes.Count - 1 do
  begin
    FRootNodes[i].WriteStream(S);
  end;

  DoProgress(S.Size);

end;

function TCustomXmlEx.WriteToString: Utf8String;
var
  S: TsdUTF8StringStream;
begin
  S := TsdUTF8StringStream.Create('');
  try
    WriteStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

{ TNativeXmlEx }

function TNativeXmlEx.NodeNew(const AName: Utf8String): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], []);
end;

function TNativeXmlEx.NodeNew(const AName: Utf8String; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], SubNodes);
end;

function TNativeXmlEx.NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], []);
end;

function TNativeXmlEx.NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXmlEx.NodeNewInt(AName: Utf8String; AValue: integer): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, IntToStr(AValue), xeElement, [], []);
end;

function TNativeXmlEx.NodeNewText(AName, AValue: Utf8String): TXmlNode;
begin
//todo
  //Result := NodeNewTextTypeAttr(AName, AValue, xeElement, [], []
  Result := nil;
end;

function TNativeXmlEx.NodeNewTextTypeAttr(AName, AValue: Utf8String;
  AElementType: TsdElementType; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
// todo
  Result := nil;
end;

function TNativeXmlEx.NodeNewTextTypeAttrEx(AName, AValue: Utf8String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
// todo
  //Result := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes);
  Result := nil;
end;

function TNativeXmlEx.NodeNewTextTypeAttrEx(AName, AValue: Utf8String;
  AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes, SubNodes);
end;

end.
