{ unit RelaxXml

  This is a small-footprint implementation to read and write XML documents
  natively from Delpi code.

  You can use this code to read XML documents from files, streams or strings.
  The load routine generates events that can be used to display load progress
  on the fly.

  Note: any external encoding (ANSI, UTF16, etc) is converted to an internal
  encoding that is ANSI or UTF8. When the loaded document is ANSI based,
  the encoding will be ANSI, in other cases (UTF8, UTF16) the encoding
  will be UTF8.

  Original Author: Nils Haeck M.Sc. (n.haeck@simdesign.nl)
  Original Date: 01 Apr 2003
  Version: see below
  Copyright (c) 2003-2010 Simdesign BV
  Contributor(s): Stefan Glienke

  It is NOT allowed under ANY circumstances to publish or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit RelaxXml;

interface

{$i relaxxml.inc}

uses
  Windows,
  Graphics,
  Classes,
  SysUtils;

const

  // Current version of the NativeXml unit
  cRelaxXmlVersion = '1.00';

// cross-platform pointer type
type
  TPointer = Pointer;

// Delphi 2007 and below
type
  UnicodeString = WideString;
  UnicodeChar = WideChar;
  PUnicodeChar = PWideChar;
  RawByteString = AnsiString;

type

  // Note on TRelaxXml.Format:
  // - xfReadable (default) to be able to read the xml file with a standard editor.
  // - xfCompact to save the xml fully compliant and at smallest size
  TXmlFormatType = (
    xfReadable,   // Save in readable format with CR-LF and indents
    xfCompact     // Save without any control chars except LF after declarations
  );

  // TXmlElementType enumerates the different kinds of elements that can be found
  // in the XML document.
  TXmlElementType = (
    xeNormal,      // Normal element <name {attr}>[value][sub-elements]</name>
    xeComment,     // Comment <!--{comment}-->
    xeCData,       // literal data <![CDATA[{data}]]>
    xeDeclaration, // XML declaration <?xml{declaration}?>
    xeStylesheet,  // Stylesheet <?xml-stylesheet{stylesheet}?>
    xeDoctype,     // DOCTYPE DTD declaration <!DOCTYPE{spec}>
    xeElement,     // <!ELEMENT >
    xeAttList,     // <!ATTLIST >
    xeEntity,      // <!ENTITY >
    xeNotation,    // <!NOTATION >
    xeExclam,      // Any <!data>
    xeQuestion,    // Any <?data?>
    xeCharData,    // character data in a node
    xeUnknown      // Any <data>
  );

  // Choose what kind of binary encoding will be used when calling
  // TXmlNode BufferRead and BufferWrite.
  TBinaryEncodingType = (
    xbeBinHex,  { With this encoding, each byte is stored as a hexadecimal
                  number, e.g. 0 = 00 and 255 = FF.                        }
    xbeBase64   { With this encoding, each group of 3 bytes are stored as 4
                  characters, requiring 64 different AnsiCharacters.}
  );

  // Definition of different methods of String encoding.
  TStringEncodingType = (
    seAnsi,      // General 8 bit encoding, encoding must be determined from encoding declaration
    seUCS4BE,    // UCS-4 Big Endian
    seUCS4LE,    // UCS-4 Little Endian
    seUCS4_2143, // UCS-4 unusual octet order (2143)
    seUCS4_3412, // UCS-4 unusual octet order (3412)
    se16BitBE,   // General 16 bit Big Endian, encoding must be determined from encoding declaration
    se16BitLE,   // General 16 bit Little Endian, encoding must be determined from encoding declaration
    seUTF8,      // UTF-8
    seUTF16BE,   // UTF-16 Big Endian
    seUTF16LE,   // UTF-16 Little Endian
    seEBCDIC     // EBCDIC flavour
  );

  TXmlCompareOption = (
    xcNodeName,
    xcNodeType,
    xcNodeValue,
    xcAttribCount,
    xcAttribNames,
    xcAttribValues,
    xcChildCount,
    xcChildNames,
    xcChildValues,
    xcRecursive
  );

  TXmlCompareOptions = set of TXmlCompareOption;

const

  xcAll: TXmlCompareOptions = [xcNodeName, xcNodeType, xcNodeValue, xcAttribCount,
    xcAttribNames, xcAttribValues, xcChildCount, xcChildNames, xcChildValues,
    xcRecursive];

var

  // XML Defaults

  cDefaultEncodingString:          UTF8String          = 'UTF-8';
  cDefaultExternalEncoding:        TStringEncodingType = seUTF8;
  cDefaultVersionString:           UTF8String          = '1.0';
  cDefaultXmlFormat:               TXmlFormatType      = xfCompact;
  cDefaultWriteOnDefault:          boolean             = True;
  cDefaultBinaryEncoding:          TBinaryEncodingType = xbeBase64;
  cDefaultIndentString:            UTF8String          = '  ';
  cDefaultDropCommentsOnParse:     boolean             = False;
  cDefaultUseFullNodes:            boolean             = False;
  cDefaultUseLocalBias:            boolean             = False;
  cDefaultFloatAllowScientific:    boolean             = True;
  cDefaultFloatSignificantDigits:  integer             = 6;

type

  TXmlNode = class;
  TRelaxXml = class;
  TsdCodecStream = class;

  // An event that is based on the TXmlNode object Node.
  TXmlNodeEvent = procedure(Sender: TObject; Node: TXmlNode) of object;

  // An event that is used to indicate load or save progress.
  TXmlProgressEvent = procedure(Sender: TObject; Size: int64) of object;

  // This event is used in the TNativeXml.OnNodeCompare event, and should
  // return -1 if Node1 < Node2, 0 if Node1 = Node2 and 1 if Node1 > Node2.
  TXmlNodeCompareEvent = function(Sender: TObject; Node1, Node2: TXmlNode; Info: TPointer): integer of object;

  // Pass a function of this kind to TXmlNode.SortChildNodes. The function should
  // return -1 if Node1 < Node2, 0 if Node1 = Node2 and 1 if Node1 > Node2.
  TXMLNodeCompareFunction = function(Node1, Node2: TXmlNode; Info: TPointer): integer;

  // Very simple autonomous stringlist that holds the list of attributes in the node
  TsdUTF8StringList = class(TPersistent)
  private
    FItems: array of UTF8String;
    FCount: integer;
    function GetItems(Index: integer): UTF8String;
    procedure SetItems(Index: integer; const Value: UTF8String);
    function GetValues(const Name: UTF8String): UTF8String;
    function GetNames(Index: integer): UTF8String;
    procedure SetValues(const Name, Value: UTF8String);
    function GetText: UTF8String;
  public
    function Add(const S: UTF8String): integer;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOfName(const Name: UTF8String): integer;
    property Count: integer read FCount;
    property Items[Index: integer]: UTF8String read GetItems write SetItems; default;
    property Names[Index: integer]: UTF8String read GetNames;
    property Values[const Name: UTF8String]: UTF8String read GetValues write SetValues;
    property Text: UTF8String read GetText;
  end;

  // The TXmlNode represents an element in the XML file. Each TNativeXml holds
  // one Root element. Under ths root element, sub-elements can be nested (there
  // is no limit on how deep). Property ElementType defines what kind of element
  // this node is.
  TXmlNode = class(TPersistent)
  private
    FName: UTF8String;              // The element name
    FValue: UTF8String;             // The *escaped* value
    FAttributes: TsdUTF8StringList; // List with attributes
    FNodes: TList;                  // These are the child elements
    FParent: TXmlNode;              // Pointer to parent element
    FDocument: TRelaxXml;          // Pointer to parent XmlDocument
    FElementType: TXmlElementType;  // The type of element
    FTag: integer;                  // A value the developer can use
    function AbortParsing: boolean;
    function GetValueAsString: UTF8String;
    procedure SetAttributeName(Index: integer; const Value: UTF8String);
    procedure SetAttributeValue(Index: integer; const Value: UTF8String);
    procedure SetValueAsString(const AValue: UTF8String);
    function GetIndent: UTF8String;
    function GetLineFeed: UTF8String;
    function GetTreeDepth: integer;
    function GetAttributeCount: integer;
    function GetAttributePair(Index: integer): UTF8String;
    function GetAttributeName(Index: integer): UTF8String;
    function GetAttributeValue(Index: integer): UTF8String;
    function GetWriteOnDefault: boolean;
    function GetBinaryEncoding: TBinaryEncodingType;
    function GetCascadedName: UTF8String;
    function QualifyAsDirectNode: boolean;
    procedure SetName(const Value: UTF8String);
    function GetFullPath: UTF8String;
    procedure SetBinaryEncoding(const Value: TBinaryEncodingType);
    function GetBinaryString: RawByteString;
    procedure SetBinaryString(const Value: RawByteString);
    function UseFullNodes: boolean;
    function UseLocalBias: Boolean;
    function GetValueAsUnicodeString: UnicodeString;
    procedure SetValueAsUnicodeString(const Value: UnicodeString);
    function GetAttributeByName(const AName: UTF8String): UTF8String;
    procedure SetAttributeByName(const AName, Value: UTF8String);
    function GetValueAsInteger: integer;
    procedure SetValueAsInteger(const Value: integer);
    function GetValueAsFloat: double;
    procedure SetValueAsFloat(const Value: double);
    function GetValueAsDateTime: TDateTime;
    procedure SetValueAsDateTime(const Value: TDateTime);
    function GetValueAsBool: boolean;
    procedure SetValueAsBool(const Value: boolean);
    function GetValueAsInt64: int64;
    procedure SetValueAsInt64(const Value: int64);
    procedure CheckCreateAttributesList;
    function GetAttributeValueAsUnicodeString(Index: integer): UnicodeString;
    procedure SetAttributeValueAsUnicodeString(Index: integer;
      const Value: UnicodeString);
    function GetAttributeValueAsInteger(Index: integer): integer;
    procedure SetAttributeValueAsInteger(Index: integer;
      const Value: integer);
    function GetAttributeByNameWide(const AName: UTF8String): UnicodeString;
    procedure SetAttributeByNameWide(const AName: UTF8String;
      const Value: UnicodeString);
    function GetTotalNodeCount: integer;
    function FloatSignificantDigits: integer;
    function FloatAllowScientific: boolean;
    function GetAttributeValueDirect(Index: integer): UTF8String;
    procedure SetAttributeValueDirect(Index: integer; const Value: UTF8String);
  protected
    function CompareNodeName(const NodeName: UTF8String): integer;
    procedure DeleteEmptyAttributes;
    function GetNodes(Index: integer): TXmlNode; virtual;
    function GetNodeCount: integer; virtual;
    procedure ParseTag(const AValue: UTF8String; TagStart, TagClose: integer);
    procedure ReadFromStream(S: TStream); virtual;
    procedure ReadFromString(const AValue: UTF8String); virtual;
    procedure ResolveEntityReferences;
    function UnescapeString(const AValue: UTF8String): UTF8String; virtual;
    function WriteInnerTag: UTF8String; virtual;
    procedure WriteToStream(S: TStream); virtual;
    procedure ChangeDocument(ADocument: TRelaxXml);
  public
    // Create a new TXmlNode object. ADocument must be the TNativeXml that is
    // going to hold this new node.
    constructor Create(ADocument: TRelaxXml); virtual;
    // Create a new TXmlNode with name AName. ADocument must be the TNativeXml
    // that is going to hold this new node.
    constructor CreateName(ADocument: TRelaxXml; const AName: UTF8String); virtual;
    // Create a new TXmlNode with name AName and UTF8String value AValue. ADocument
    // must be the TNativeXml that is going to hold this new node.
    constructor CreateNameValue(ADocument: TRelaxXml; const AName, AValue: UTF8String); virtual;
    // Create a new TXmlNode with XML element type AType. ADocument must be the
    // TRelaxXml that is going to hold this new node.
    constructor CreateType(ADocument: TRelaxXml; AType: TXmlElementType); virtual;
    // Use Assign to assign another TXmlNode to this node. This means that all
    // properties and subnodes from the Source TXmlNode are copied to the current
    // node. You can also Assign a TNativeXml document to the node, in that case
    // the RootNodeList property of the TNativeXml object will be copied.
    procedure Assign(Source: TPersistent); override;
    // Call Delete to delete this node completely from the parent node list. This
    // call only succeeds if the node has a parent. It has no effect when called for
    // the root node.
    procedure Delete; virtual;
    // Delete all nodes that are empty (this means, which have no subnodes, no
    // attributes, and no value assigned). This procedure works recursively.
    procedure DeleteEmptyNodes;
    // Destroy a TXmlNode object. This will free the child node list automatically.
    // Never call this method directly. All TXmlNodes in the document will be
    // recursively freed when TNativeXml.Free is called.
    destructor Destroy; override;
    // Use this method to add an integer attribute to the node.
    procedure AttributeAdd(const AName: UTF8String; AValue: integer); overload;
    // Use this method to add a string attribute with value AValue to the node.
    procedure AttributeAdd(const AName, AValue: UTF8String); overload;
    // Use this method to delete the attribute at Index in the list. Index must be
    // equal or greater than 0, and smaller than AttributeCount. Using an index
    // outside of that range has no effect.
    procedure AttributeDelete(Index: integer);
    // Switch position of the attributes at Index1 and Index2.
    procedure AttributeExchange(Index1, Index2: integer);
    // Use this method to find the index of an attribute with name AName.
    function AttributeIndexByname(const AName: UTF8String): integer;
    // Clear all attributes from the current node.
    procedure AttributesClear; virtual;
    // Use this method to read binary data from the node into Buffer with a length of Count.
    procedure BufferRead(var Buffer; Count: Integer); virtual;
    // Use this method to write binary data in Buffer with a length of Count to the
    // current node. The data will appear as text using either BinHex or Base64
    // method) in the final XML document.
    // Notice that NativeXml does only support up to 2Gb bytes of data per file,
    // so do not use this option for huge files.
    procedure BufferWrite(const Buffer; Count: Integer); virtual;
    // Returns the length of the data in the buffer, once it would be decoded by
    // method xbeBinHex or xbeBase64.
    function BufferLength: integer; virtual;
    // Clear all child nodes and attributes, and the name and value of the current
    // XML node. However, the node is not deleted. Call Delete instead for that.
    procedure Clear; virtual;
    // Find the first node which has name NodeName. Contrary to the NodeByName
    // function, this function will search the whole subnode tree, using the
    // DepthFirst method. It is possible to search for a full path too, e.g.
    // FoundNode := MyNode.FindNode('/Root/SubNode1/SubNode2/ThisNode');
    function FindNode(const NodeName: UTF8String): TXmlNode;
    // Find all nodes which have name NodeName. Contrary to the NodesByName
    // function, this function will search the whole subnode tree. If you use
    // a TXmlNodeList for the AList parameter, you don't need to cast the list
    // items to TXmlNode.
    procedure FindNodes(const NodeName: UTF8String; const AList: TList);
    // Use FromAnsiString to convert a normal ANSI String to a UTF8String for the node
    // (name, value, attributes). In TNativeXml the ANSI Characters are encoded
    // into UTF8.
    function FromAnsiString(const s: AnsiString): UTF8String;
    // Use FromUnicodeString to convert UnicodeString to a UTF8String for the node (name, value,
    // attributes).
    function FromUnicodeString(const W: UnicodeString): UTF8String;
    // Use HasAttribute to determine if the node has an attribute with name AName.
    function HasAttribute(const AName: UTF8String): boolean; virtual;
    // This function returns the index of this node in the parent's node list.
    // If Parent is not assigned, this function returns -1.
    function IndexInParent: integer;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node Name and value are empty.
    function IsClear: boolean; virtual;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node value is empty.
    function IsEmpty: boolean; virtual;
    function IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList = nil): boolean;
    // Add the node ANode as a new subelement in the nodelist. The node will be
    // added in position NodeCount (which will be returned).
    function NodeAdd(ANode: TXmlNode): integer; virtual;
    // This function returns a pointer to the first subnode that has an attribute with
    // name AttribName and value AttribValue. If ShouldRecurse = True (default), the
    // function works recursively, using the depthfirst method.
    function NodeByAttributeValue(const NodeName, AttribName, AttribValue: UTF8String;
      ShouldRecurse: boolean = True): TXmlNode;
    // Return a pointer to the first subnode with this Elementype, or return nil
    // if no subnode with that type is found.
    function NodeByElementType(ElementType: TXmlElementType): TXmlNode;
    // Return a pointer to the first subnode in the nodelist that has name AName.
    // If no subnodes with AName are found, the function returns nil.
    function NodeByName(const AName: UTF8String): TXmlNode; virtual;
    // Delete the subnode at Index. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeDelete(Index: integer); virtual;
    // Switch position of the nodes at Index1 and Index2.
    procedure NodeExchange(Index1, Index2: integer);
    // Extract the node ANode from the subnode list. The node will no longer appear
    // in the subnodes list, so the application is responsible for freeing ANode later.
    function NodeExtract(ANode: TXmlNode): TXmlNode; virtual;
    // This function returns a pointer to the first node with AName. If this node
    // is not found, then it creates a new node with AName and returns its pointer.
    function NodeFindOrCreate(const AName: UTF8String): TXmlNode; virtual;
    // Find the index of the first subnode with name AName.
    function NodeIndexByName(const AName: UTF8String): integer; virtual;
    // Find the index of the first subnode with name AName that appears after or on
    // the index AFrom. This function can be used in a loop to retrieve all nodes
    // with a certain name, without using a helper list. See also NodesByName.
    function NodeIndexByNameFrom(const AName: UTF8String; AFrom: integer): integer; virtual;
    // Call NodeIndexOf to get the index for ANode in the Nodes array. The first
    // node in the array has index 0, the second item has index 1, and so on. If
    // a node is not in the list, NodeIndexOf returns -1.
    function NodeIndexOf(ANode: TXmlNode): integer;
    // Insert the node ANode at location Index in the list.
    procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
    // Create a new node with AName, add it to the subnode list, and return a
    // pointer to it.
    function NodeNew(const AName: UTF8String): TXmlNode; virtual;
    // Create a new node with AName, and insert it into the subnode list at location
    // Index, and return a pointer to it.
    function NodeNewAtIndex(Index: integer; const AName: UTF8String): TXmlNode; virtual;
    // Call NodeRemove to remove a specific node from the Nodes array when its index
    // is unknown. The value returned is the index of the item in the Nodes array
    // before it was removed. After an item is removed, all the items that follow
    // it are moved up in index position and the NodeCount is reduced by one.
    function NodeRemove(ANode: TxmlNode): integer;
    // Clear (and free) the complete list of subnodes.
    procedure NodesClear; virtual;
    // Use this procedure to retrieve all nodes that have name AName. Pointers to
    // these nodes are added to the list in AList. AList must be initialized
    // before calling this procedure. If you use a TXmlNodeList you don't need
    // to cast the list items to TXmlNode.
    procedure NodesByName(const AName: UTF8String; const AList: TList);
    // Find the attribute with AName, and convert its value to a boolean. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeBool(const AName: UTF8String; ADefault: boolean = False): boolean; virtual;
    function ReadAttributeDateTime(const AName: UTF8String; ADefault: TDateTime = 0): TDateTime; virtual;
    // Find the attribute with AName, and convert its value to an integer. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeInteger(const AName: UTF8String; ADefault: integer = 0): integer; virtual;
    // Find the attribute with AName, and convert its value to an int64. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeInt64(const AName: UTF8String; ADefault: int64 = 0): int64; virtual;
    // Find the attribute with AName, and convert its value to a float. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeFloat(const AName: UTF8String; ADefault: double = 0): double;
    function ReadAttributeString(const AName: UTF8String; const ADefault: UTF8String = ''): UTF8String; virtual;
    // Read the subnode with AName and convert it to a boolean value. If the
    // subnode is not found, or cannot be converted, the boolean ADefault will
    // be returned.
    function ReadBool(const AName: UTF8String; ADefault: boolean = False): boolean; virtual;
    // Read the properties Color and Style for the TBrush object ABrush from the
    // subnode with AName.
    procedure ReadBrush(const AName: UTF8String; ABrush: TBrush); virtual;
    // Read the subnode with AName and convert its value to TColor. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadColor(const AName: UTF8String; ADefault: TColor = clBlack): TColor; virtual;
    // Read the properties \Name, Color, Size and Style for the TFont object AFont
    // from the subnode with AName.
    procedure ReadFont(const AName: UTF8String; AFont: TFont); virtual;
    // Read the properties Color, Mode, Style and Width for the TPen object APen
    // from the subnode with AName.
    procedure ReadPen(const AName: UTF8String; APen: TPen); virtual;
    // Read the subnode with AName and convert its value to TDateTime. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadDateTime(const AName: UTF8String; ADefault: TDateTime = 0): TDateTime; virtual;
    // Read the subnode with AName and convert its value to a double. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadFloat(const AName: UTF8String; ADefault: double = 0.0): double; virtual;
    // Read the subnode with AName and convert its value to an int64. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadInt64(const AName: UTF8String; ADefault: int64 = 0): int64; virtual;
    // Read the subnode with AName and convert its value to an integer. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadInteger(const AName: UTF8String; ADefault: integer = 0): integer; virtual;
    // Read the subnode with AName and return its UTF8String value. If the subnode is
    // not found, ADefault will be returned.
    function ReadString(const AName: UTF8String; const ADefault: UTF8String = ''): UTF8String; virtual;
    // Read the subnode with AName and return its UnicodeString value. If the subnode is
    // not found, ADefault will be returned.
    function ReadUnicodeString(const AName: UTF8String; const ADefault: UnicodeString = ''): UnicodeString; virtual;
    // Sort the child nodes of this node. Provide a custom node compare function in Compare,
    // or attach an event handler to the parent documents' OnNodeCompare in order to
    // provide custom sorting. If no compare function is given (nil) and OnNodeCompare
    // is not implemented, SortChildNodes will simply sort the nodes by name (ascending,
    // case insensitive). The Info pointer parameter can be used to pass any custom
    // information to the compare function. Default value for Info is nil.
    procedure SortChildNodes(Compare: TXMLNodeCompareFunction = nil; Info: TPointer = nil);
    // Use ToUnicodeString to convert any UTF8 String from the node (name, value, attributes)
    // to a UnicodeString.
    function ToUnicodeString(const s: UTF8String): UnicodeString;
    // Convert the node's value to boolean and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsBoolDef(ADefault: boolean): boolean; virtual;
    // Convert the node's value to a TDateTime and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsDateTimeDef(ADefault: TDateTime): TDateTime; virtual;
    // Convert the node's value to a double and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsFloatDef(ADefault: double): double; virtual;
    // Convert the node's value to int64 and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsInt64Def(ADefault: int64): int64; virtual;
    // Convert the node's value to integer and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function ValueAsIntegerDef(ADefault: integer): integer; virtual;
    // If the attribute with name AName exists, then set its value to the boolean
    // AValue. If it does not exist, then create a new attribute AName with the
    // boolean value converted to either "True" or "False". If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeBool(const AName: UTF8String; AValue: boolean; ADefault: boolean = False); virtual;
    procedure WriteAttributeDateTime(const AName: UTF8string; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
    // If the attribute with name AName exists, then set its value to the integer
    // AValue. If it does not exist, then create a new attribute AName with the
    // integer value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeInteger(const AName: UTF8String; AValue: integer; ADefault: integer = 0); virtual;
    procedure WriteAttributeInt64(const AName: UTF8String; const AValue: int64; ADefault: int64 = 0); virtual;
    procedure WriteAttributeFloat(const AName: UTF8String; AValue: double; ADefault: double = 0); virtual;
    // If the attribute with name AName exists, then set its value to the UTF8String
    // AValue. If it does not exist, then create a new attribute AName with the
    // value AValue. If ADefault = AValue, and WriteOnDefault = False, no attribute
    // will be added.
    procedure WriteAttributeString(const AName: UTF8String; const AValue: UTF8String; const ADefault: UTF8String = ''); virtual;
    // Add or replace the subnode with AName and set its value to represent the boolean
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteBool(const AName: UTF8String; AValue: boolean; ADefault: boolean = False); virtual;
    // Write properties Color and Style of the TBrush object ABrush to the subnode
    // with AName. If AName does not exist, it will be created.
    procedure WriteBrush(const AName: UTF8String; ABrush: TBrush); virtual;
    // Add or replace the subnode with AName and set its value to represent the TColor
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteColor(const AName: UTF8String; AValue: TColor; ADefault: TColor = clBlack); virtual;
    // Write properties \Name, Color, Size and Style of the TFont object AFont to
    // the subnode with AName. If AName does not exist, it will be created.
    procedure WriteFont(const AName: UTF8String; AFont: TFont); virtual;
    // Write properties Color, Mode, Style and Width of the TPen object APen to
    // the subnode with AName. If AName does not exist, it will be created.
    procedure WritePen(const AName: UTF8String; APen: TPen); virtual;
    // Add or replace the subnode with AName and set its value to represent the TDateTime
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    // The XML format used is compliant with W3C's specification of date and time.
    procedure WriteDateTime(const AName: UTF8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the double
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteFloat(const AName: UTF8String; AValue: double; ADefault: double = 0.0); virtual;
    // Add or replace the subnode with AName and set its value to represent the hexadecimal representation of
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteHex(const AName: UTF8String; AValue: integer; Digits: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the int64
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInt64(const AName: UTF8String; AValue: int64; ADefault: int64 = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the integer
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInteger(const AName: UTF8String; AValue: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the UTF8String
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteString(const AName, AValue: UTF8String; const ADefault: UTF8String = ''); virtual;
    // Call WriteToString to save the XML node to a UTF8String. This method can be used to store
    // individual nodes instead of the complete XML document.
    function WriteToString: UTF8String; virtual;
    // Add or replace the subnode with AName and set its value to represent the UnicodeString
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteUnicodeString(const AName: UTF8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
    // AttributeByName returns the attribute value for the attribute that has name AName.
    // Set AttributeByName to add an attribute to the attribute list, or replace an
    // existing one.
    property AttributeByName[const AName: UTF8String]: UTF8String read GetAttributeByName write
      SetAttributeByName;
    // AttributeByNameWide returns the attribute value for the attribute that has name AName
    // as UnicodeString. Set AttributeByNameWide to add an attribute to the attribute list, or replace an
    // existing one.
    property AttributeByNameWide[const AName: UTF8String]: UnicodeString read GetAttributeByNameWide write
      SetAttributeByNameWide;
    // Returns the number of attributes in the current node.
    property AttributeCount: integer read GetAttributeCount;
    // Read this property to get the name of the attribute at Index. Note that Index
    // is zero-based: Index goes from 0 to AttributeCount - 1
    property AttributeName[Index: integer]: UTF8String read GetAttributeName write SetAttributeName;
    // Read this property to get the Attribute \Name and Value pair at index Index.
    // This is a UTF8String with \Name and Value separated by a TAB character (#9).
    property AttributePair[Index: integer]: UTF8String read GetAttributePair;
    // Read this property to get the UTF8String value of the attribute at index Index.
    // Write to it to set the UTF8String value.
    property AttributeValue[Index: integer]: UTF8String read GetAttributeValue write SetAttributeValue;
    // Read this property to get the UnicodeString value of the attribute at index Index.
    // Write to it to set the UnicodeString value.
    property AttributeValueAsUnicodeString[Index: integer]: UnicodeString read GetAttributeValueAsUnicodeString write SetAttributeValueAsUnicodeString;
    // Read this property to get the integer value of the attribute at index Index.
    // If the value cannot be converted, 0 will be returned. Write to it to set the integer value.
    property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
    // Set or get the raw attribute value, thus circumventing the escape function. Make sure that
    // the value you set does not contain the & and quote AnsiCharacters, or the produced
    // XML will be invalid.
    property AttributeValueDirect[Index: integer]: UTF8String read GetAttributeValueDirect write SetAttributeValueDirect;
    // BinaryEncoding reflects the same value as the BinaryEncoding setting of the parent
    // Document.
    property BinaryEncoding: TBinaryEncodingType read GetBinaryEncoding write SetBinaryEncoding;
    // Use BinaryString to add/extract binary data in an easy way to/from the node. Internally the
    // data gets stored as Base64-encoded data. Do not use this method for normal textual
    // information, it is better to use ValueAsString in that case (adds less overhead).
    property BinaryString: RawByteString read GetBinaryString write SetBinaryString;
    // This property returns the name and index and all predecessors with underscores
    // to separate, in order to get a unique reference that can be used in filenames.
    property CascadedName: UTF8String read GetCascadedName;
    // Pointer to parent NativeXml document, or Nil if none.
    property Document: TRelaxXml read FDocument write FDocument;
    // ElementType contains the type of element that this node holds.
    property ElementType: TXmlElementType read FElementType write FElementType;
    // Fullpath will return the complete path of the node from the root, e.g.
    // /Root/SubNode1/SubNode2/ThisNode
    property FullPath: UTF8String read GetFullPath;
    // Read Name to get the name of the element, and write Name to set the name.
    // This is the full name and may include a namespace. (Namespace:Name)
    property Name: UTF8String read FName write SetName;
    // Parent points to the parent node of the current XML node.
    property Parent: TXmlNode read FParent write FParent;
    // NodeCount is the number of child nodes that this node holds.
    property NodeCount: integer read GetNodeCount;
    // Use Nodes to access the child nodes of the current XML node by index. Note
    // that the list is zero-based, so Index is valid from 0 to NodeCount - 1.
    property Nodes[Index: integer]: TXmlNode read GetNodes; default;
    // Tag is an integer value the developer can use in any way. Tag does not get
    // saved to the XML. Tag is often used to point to a GUI element (and is then
    // cast to a pointer).
    property Tag: integer read FTag write FTag;
    // TotalNodeCount represents the total number of child nodes, and child nodes
    // of child nodes etcetera of this particular node.
    property TotalNodeCount: integer read GetTotalNodeCount;
    // Read TreeDepth to find out many nested levels there are for the current XML
    // node. Root has a TreeDepth of zero.
    property TreeDepth: integer read GetTreeDepth;
    // ValueAsBool returns the node's value as boolean, or raises an
    // exception if the value cannot be converted to boolean. Set ValueAsBool
    // to convert a boolean to a UTF8String in the node's value field. See also
    // function ValueAsBoolDef.
    property ValueAsBool: boolean read GetValueAsBool write SetValueAsBool;
    // ValueAsDateTime returns the node's value as TDateTime, or raises an
    // exception if the value cannot be converted to TDateTime. Set ValueAsDateTime
    // to convert a TDateTime to a UTF8String in the node's value field. See also
    // function ValueAsDateTimeDef.
    property ValueAsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
    // ValueAsIn64 returns the node's value as int64, or raises an
    // exception if the value cannot be converted to int64. Set ValueAsInt64
    // to convert an int64 to a UTF8String in the node's value field. See also
    // function ValueAsInt64Def.
    property ValueAsInt64: int64 read GetValueAsInt64 write SetValueAsInt64;
    // ValueAsInteger returns the node's value as integer, or raises an
    // exception if the value cannot be converted to integer. Set ValueAsInteger
    // to convert an integer to a UTF8String in the node's value field. See also
    // function ValueAsIntegerDef.
    property ValueAsInteger: integer read GetValueAsInteger write SetValueAsInteger;
    // ValueAsFloat returns the node's value as float, or raises an
    // exception if the value cannot be converted to float. Set ValueAsFloat
    // to convert a float to a UTF8String in the node's value field. See also
    // function ValueAsFloatDef.
    property ValueAsFloat: double read GetValueAsFloat write SetValueAsFloat;
    // ValueAsString returns the unescaped version of ValueDirect. All neccesary
    // characters in ValueDirect must be escaped (e.g. "&" becomes "&amp;") but
    // ValueAsString returns them in original format. Always use ValueAsString to
    // set the text value of a node, to make sure all neccesary charaters are
    // escaped.
    property ValueAsString: UTF8String read GetValueAsString write SetValueAsString;
    // ValueAsUnicodeString returns the unescaped version of ValueDirect as a UnicodeString.
    // Always use ValueAsUnicodeString to set the text value of a node, to make sure all
    // neccesary charaters are escaped.
    property ValueAsUnicodeString: UnicodeString read GetValueAsUnicodeString write SetValueAsUnicodeString;
    // ValueDirect is the exact text value as was parsed from the stream. If multiple
    // text elements are encountered, they are added to ValueDirect with a CR to
    // separate them.
    property ValueDirect: UTF8String read FValue write FValue;
    // WriteOnDefault reflects the same value as the WriteOnDefault setting of the parent
    // Document.
    property WriteOnDefault: boolean read GetWriteOnDefault;
  end;

  // TXmlNodeList is a utility TList descendant that can be used to work with selection
  // lists.
  TXmlNodeList = class(TList)
  private
    function GetItems(Index: Integer): TXmlNode;
    procedure SetItems(Index: Integer; const Value: TXmlNode);
  public
    // Return the first node in the list that has an attribute with AName, AValue
    function ByAttribute(const AName, AValue: UTF8String): TXmlNode;
    property Items[Index: Integer]: TXmlNode read GetItems write SetItems; default;
  end;

  // TRelaxXml is the XML document holder. Create a TRelaxXml and then use
  // methods LoadFromFile, LoadFromStream or ReadFromString to load an XML document
  // into memory.
  TRelaxXml = class(TPersistent)
  private
    FAbortParsing: boolean;         // Signal to abort the parsing process
    FBinaryEncoding: TBinaryEncodingType; // xbeBinHex or xbeBase64
    FCodecStream: TsdCodecStream;   // Temporary stream used to read encoded files
    FDropCommentsOnParse: boolean;  // If true, comments are dropped (deleted) when parsing
    FExternalEncoding: TStringEncodingType;
    FFloatAllowScientific: boolean;
    FFloatSignificantDigits: integer;
    FParserWarnings: boolean;       // Show parser warnings for non-critical errors
    FRootNodes: TXmlNode;           // Root nodes in the document (which contains one normal element that is the root)
    FIndentString: UTF8String;      // The indent string used to indent content (default is two spaces)
    FUseFullNodes: boolean;         // If true, nodes are never written in short notation.
    FUseLocalBias: Boolean;         // If true, datetime values are written with timezone offset and converted to local time when read
    FWriteOnDefault: boolean;       // Set this option to "False" to only write values <> default value (default = true)
    FXmlFormat: TXmlFormatType;     // xfReadable, xfCompact
    FOnNodeCompare: TXmlNodeCompareEvent; // Compare two nodes
    FOnNodeNew: TXmlNodeEvent;      // Called after a node is added
    FOnNodeLoaded: TXmlNodeEvent;   // Called after a node is loaded completely
    FOnProgress: TXmlProgressEvent; // Called after a node is loaded/saved, with the current position in the file
    FOnUnicodeLoss: TNotifyEvent;   // This event is called when there is a warning for unicode conversion loss when reading unicode
    procedure DoNodeNew(Node: TXmlNode);
    procedure DoNodeLoaded(Node: TXmlNode);
    procedure DoUnicodeLoss(Sender: TObject);
    function GetCommentString: UTF8String;
    procedure SetCommentString(const Value: UTF8String);
    function GetEntityByName(AName: UTF8String): UTF8String;
    function GetRoot: TXmlNode;
    function GetCharSet: UTF8String;
    procedure SetCharSet(const Value: UTF8String);
    function GetVersionString: UTF8String;
    procedure SetVersionString(const Value: UTF8String);
    function GetStyleSheetNode: TXmlNode;
    function GetUtf8Encoded: boolean;
  protected
    procedure CopyFrom(Source: TRelaxXml); virtual;
    procedure DoProgress(Size: integer);
    function LineFeed: UTF8String; virtual;
    procedure ParseDTD(ANode: TXmlNode; S: TStream); virtual;
    procedure ReadFromStream(S: TStream); virtual;
    procedure WriteToStream(S: TStream); virtual;
    procedure SetDefaults; virtual;
  public
    // Create a new NativeXml document which can then be used to read or write XML files.
    // A document that is created with Create must later be freed using Free.
    constructor Create; virtual;
    // Use CreateName to Create a new Xml document that will automatically
    // contain a root element with name ARootName.
    constructor CreateName(const ARootName: UTF8String); virtual;
    // Destroy will free all data in the TNativeXml object. This includes the
    // root node and all subnodes under it. Do not call Destroy directly, call
    // Free instead.
    destructor Destroy; override;
    // create a new document
    constructor New;
    // When calling Assign with a Source object that is a TNativeXml, will cause
    // it to copy all data from Source.
    procedure Assign(Source: TPersistent); override;
    // Call Clear to remove all data from the object, and restore all defaults.
    procedure Clear; virtual;
    // canonicalize the component 
    procedure Canonicalize;
    // Function IsEmpty returns true if the root is clear, or in other words, the
    // root contains no value, no name, no subnodes and no attributes.
    function IsEmpty: boolean; virtual;
    // Load an XML document from the TStream object in Stream. The LoadFromStream
    // procedure will raise an exception of type EFilerError when it encounters
    // non-wellformed XML. This method can be used with any TStream descendant.
    // See also LoadFromFile and ReadFromString.
    procedure LoadFromStream(Stream: TStream); virtual;
    // Call procedure LoadFromFile to load an XML document from the filename
    // specified. See Create for an example. The LoadFromFile procedure will raise
    // an exception of type EFilerError when it encounters non-wellformed XML.
    procedure LoadFromFile(const AFileName: string); virtual;
    // Call procedure ReadFromString to load an XML document from the UTF8String AValue.
    // The ReadFromString procedure will raise an exception of type EFilerError
    // when it encounters non-wellformed XML.
    procedure ReadFromString(const AValue: UTF8String); virtual;
    // Call ResolveEntityReferences after the document has been loaded to resolve
    // any present entity references (&Entity;). When an entity is found in the
    // DTD, it will replace the entity reference. Whenever an entity contains
    // XML markup, it will be parsed and become part of the document tree. Since
    // calling ResolveEntityReferences is adding quite some extra overhead, it
    // is not done automatically. If you want to do the entity replacement, a good
    // moment to call ResolveEntityReferences is right after LoadFromFile.
    procedure ResolveEntityReferences;
    // Call SaveToStream to save the XML document to the Stream. Stream
    // can be any TStream descendant. Set XmlFormat to xfReadable if you want
    // the stream to contain indentations to make the XML more human-readable. This
    // is not the default and also not compliant with the XML specification. See
    // SaveToFile for information on how to save in special encoding.
    procedure SaveToStream(Stream: TStream); virtual;
    // Call SaveToFile to save the XML document to a file with FileName.
    procedure SaveToFile(const AFileName: string); virtual;
    // Call WriteToString to save the XML document to a UTF8String. Set XmlFormat to
    // xfReadable if you want the UTF8String to contain indentations to make the XML
    // more human-readable. This is not the default and also not compliant with
    // the XML specification.
    function WriteToString: UTF8String; virtual;
    // Set AbortParsing to True if you use the OnNodeNew and OnNodeLoaded events in
    // a SAX-like manner, and you want to abort the parsing process halfway.
    property AbortParsing: boolean read FAbortParsing write FAbortParsing;
    // Choose what kind of binary encoding will be used when calling TXmlNode.BufferRead
    // and TXmlNode.BufferWrite. Default value is xbeBase64.
    property BinaryEncoding: TBinaryEncodingType read FBinaryEncoding write FBinaryEncoding;
    // A comment string above the root element \<!--{comment}--\> can be accessed with
    // this property. Assign a comment to this property to add it to the XML document.
    // Use property RootNodeList to add/insert/extract multiple comments.
    property CommentString: UTF8String read GetCommentString write SetCommentString;
    // Set DropCommentsOnParse if you're not interested in any comment nodes in your object
    // model data. All comments encountered during parsing will simply be skipped and
    // not added as a node with ElementType = xeComment (which is default).
    property DropCommentsOnParse: boolean read FDropCommentsOnParse write FDropCommentsOnParse;
    // Encoding string (e.g. "UTF-8" or "UTF-16"). This encoding string is stored in
    // the header.
    property CharSet: UTF8String read GetCharSet write SetCharSet;
    // Returns the value of the named entity in Name, where name should be stripped
    // of the leading & and trailing ;. These entity values are parsed from the
    // Doctype declaration (if any).
    property EntityByName[AName: UTF8String]: UTF8String read GetEntityByName;
    // ExternalEncoding defines in which format XML files are saved. Set ExternalEncoding
    // to se8bit to save as plain text files, to seUtf8 to save as UTF8 files (with
    // Byte Order Mark #EF BB FF) and to seUTF16LE to save as unicode (Byte Order
    // Mark #FF FE). When reading an XML file, the value of ExternalEncoding will
    // be set according to the byte order mark and/or encoding declaration found.
    property ExternalEncoding: TStringEncodingType read FExternalEncoding write FExternalEncoding;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will allow to output scientific notation in some cases, if the
    // result is significantly shorter than normal output, but only if the value
    // of FloatAllowScientific is True (default).
    property FloatAllowScientific: boolean read FFloatAllowScientific write FFloatAllowScientific;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will use this number of significant digits. The default is
    // cDefaultFloatSignificantDigits, and set to 6.
    property FloatSignificantDigits: integer read FFloatSignificantDigits write FFloatSignificantDigits;
    // IndentString is the string used for indentations. By default, it is two
    // spaces: '  '. Set IndentString to something else if you need to have
    // specific indentation, or set it to an empty string to avoid indentation.
    property IndentString: UTF8String read FIndentString write FIndentString;
    // Root is the topmost element in the XML document. Access Root to read any
    // child elements. When creating a new XML document, you can automatically
    // include a Root node, by creating using CreateName.
    property Root: TXmlNode read GetRoot;
    // RootNodeList can be used to directly access the nodes in the root of the
    // XML document. Usually this list consists of one declaration node followed
    // by a normal node which is the Root. You can use this property to add or
    // delete comments, stylesheets, dtd's etc.
    property RootNodeList: TXmlNode read FRootNodes;
    // Get the stylesheet node used for this XML document. If the node does not
    // exist yet, it will be created (thus if you use this property, and don't
    // set any of the attributes, an empty stylesheet node will be the result).
    property StyleSheetNode: TXmlNode read GetStyleSheetNode;
    // Set UseFullNodes to True before saving the XML document to ensure that all
    // nodes are represented by <Node>...</Node> instead of the short version
    // <Node/>. UseFullNodes is False by default.
    property UseFullNodes: boolean read FUseFullNodes write FUseFullNodes;
    // Set UseLocalBias to True if you want to consider the local timezone bias
    // when writing and reading datetime values. UseLocalBias is False by default.
    property UseLocalBias: Boolean read FUseLocalBias write FUseLocalBias;
    // This property is here for backwards compat: all strings inside NativeXml
    // are UTF8Strings, the internal encoding is always UTF8.
    property Utf8Encoded: boolean read GetUtf8Encoded;
    // After reading, this property contains the XML version (usually "1.0").
    property VersionString: UTF8String read GetVersionString write SetVersionString;
    // Set WriteOnDefault to False if you do not want to write default values to
    // the XML document. This option can avoid creating huge documents with
    // redundant info, and will speed up writing.
    property WriteOnDefault: boolean read FWriteOnDefault write FWriteOnDefault;
    // XmlFormat by default is set to xfCompact. This setting is compliant to the spec,
    // and NativeXml will only generate XML files with #$0A as control AnsiCharacter.
    // By setting XmlFormat to xfReadable, you can generate easily readable XML
    // files that contain indentation and carriage returns after each element.
    property XmlFormat: TXmlFormatType read FXmlFormat write FXmlFormat;
    // ParserWarnings by default is True. If True, the parser will raise an
    // exception in cases where the XML document is not technically valid. If False,
    // the parser will try to ignore non-critical warnings. Set ParserWarnings
    // to False for some types of XML-based documents such as SOAP messages.
    property ParserWarnings: boolean read FParserWarnings write FParserWarnings;
    // This event is called whenever a node's SortChildNodes method is called and
    // no direct compare method is provided. Implement this event if you want to
    // use object-event based methods for comparison of nodes.
    property OnNodeCompare: TXmlNodeCompareEvent read FOnNodeCompare write FOnNodeCompare;
    // This event is called whenever the parser has encountered a new node.
    property OnNodeNew: TXmlNodeEvent read FOnNodeNew write FOnNodeNew;
    // This event is called when the parser has finished parsing the node, and
    // has created its complete contents in memory.
    property OnNodeLoaded: TXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
    // OnProgress is called during loading and saving of the XML document. The
    // Size parameter contains the position in the stream. This event can be used
    // to implement a progress indicator during loading and saving. The event is
    // called after each node that is read or written.
    property OnProgress: TXmlProgressEvent read FOnProgress write FOnProgress;
    // This event is called if there is a warning for unicode conversion loss,
    // when reading from Unicode streams or files.
    property OnUnicodeLoss: TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss;
  end;

  // This enumeration defines the conversion stream access mode.
  TsdStreamModeType = (
    umUnknown, // The stream access mode is not yet known
    umRead,    // UTF stream opened for reading
    umWrite    // UTF stream opened for writing
  );

  // TBigByteArray is an array of bytes like the standard TByteArray (windows
  // unit) but which can contain up to MaxInt bytes. This type helps avoiding
  // range check errors when working with buffers larger than 32768 bytes.
  TBigByteArray = array[0..MaxInt - 1] of byte;
  PBigByteArray = ^TBigByteArray;


  // TsdBufferedReadStream is a buffered stream that takes another TStream
  // and reads only buffer-wise from it, and reads to the stream are first
  // done from the buffer. This stream type can only support reading.
  TsdBufferedReadStream = class(TStream)
  private
    FStream: TStream;
    FBuffer: PBigByteArray;
    FPage: integer;
    FBufPos: integer;
    FBufSize: integer;
    FPosition: longint;
    FOwned: boolean;
    FMustCheck: boolean;
  protected
    procedure CheckPosition;
  public
    // Create the buffered reader stream by passing the source stream in AStream,
    // this source stream must already be initialized. If Owned is set to True,
    // the source stream will be freed by TsdBufferedReadStream.
    constructor Create(AStream: TStream; Owned: boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  // TsdBufferedWriteStream is a buffered stream that takes another TStream
  // and writes only buffer-wise to it, and writes to the stream are first
  // done to the buffer. This stream type can only support writing.
  TsdBufferedWriteStream = class(TStream)
  private
    FStream: TStream;
    FBuffer: PBigByteArray;
    FBufPos: integer;
    FPosition: longint;
    FOwned: boolean;
  protected
    procedure Flush;
  public
    // Create the buffered writer stream by passing the destination stream in AStream,
    // this destination stream must already be initialized. If Owned is set to True,
    // the destination stream will be freed by TsdBufferedWriteStream.
    constructor Create(AStream: TStream; Owned: boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;


  // TsdCodecStream is the base codec class for reading and writing encoded files.
  // See TsdUtf8Stream for more information.
  TsdCodecStream = class(TStream)
  private
    FBuffer: UTF8String;            // Buffer that holds temporary utf8 characters
    FBufferPos: integer;            // Current character in buffer
    FEncoding: TStringEncodingType; // Type of string encoding used for the external stream
    FMode: TsdStreamModeType;       // Access mode of this UTF stream, determined after first read/write
    FPosMin1: integer;              // Position for seek(-1)
    FPosMin2: integer;              // Position for seek(-2)
    FStream: TStream;               // Referenced stream
    FSwapByteOrder: boolean;
    FWarningUnicodeLoss: boolean;   // There was a warning for a unicode conversion loss
    FWriteBom: boolean;
    FOnUnicodeLoss: TNotifyEvent;   // This event is called if there is a warning for unicode conversion loss
  protected
    function ReadByte: byte; virtual;
    procedure StorePrevPositions; virtual;
    procedure WriteByte(const B: byte); virtual;
    procedure WriteBuf(const Buffer; Offset, Count: longint); virtual;
    function InternalRead(var Buffer; Offset, Count: Longint): Longint;
    function InternalSeek(Offset: Longint; Origin: TSeekOrigin): Longint;
    function InternalWrite(const Buffer; Offset, Count: Longint): Longint;
  public
    // Call Create to create a new TsdCodectream based on an input or output stream
    // in AStream. After the first Read, the input streamtype will be determined,
    // and the Encoding property will be set accordingly. When using Write to
    // write data to the referenced stream, the Encoding property must be set prior
    // to this, indicating what kind of stream to produce.
    constructor Create(AStream: TStream); virtual;
    // Read Count bytes from the referenced stream, and put them in Buffer. The function
    // returns the actual number of bytes read. The codec stream can only read
    // one byte at the time!
    function Read(var Buffer; Count: Longint): Longint; override;
    // Seek to a new position in the stream, with Origin as a reference. The codec
    // stream can not seek when writing, and when reading can only go back one
    // AnsiCharacter, or return a position. Position returned is the position
    // in the referenced stream.
    function Seek(Offset: Longint; Origin: Word): Longint; override;
     // Write Count bytes from Buffer to the referenced stream, The function
    // returns the actual number of bytes written.
    function Write(const Buffer; Count: Longint): Longint; override;
    // Set Encoding when writing to the preferred encoding of the output stream,
    // or read Encoding after reading the output stream to determine encoding type.
    property Encoding: TstringEncodingType read FEncoding write FEncoding;
    // \Read this value after loading an XML file. It will be True if there was a
    // warning for a unicode conversion loss.
    property WarningUnicodeLoss: boolean read FWarningUnicodeLoss;
    // This event is called if there is a warning for unicode conversion loss.
    property OnUnicodeLoss: TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss;
  end;

  // TsdUdf8tream is a conversion stream that will load ANSI, UTF8 or
  // Unicode files and convert them into UTF8 only. The stream can
  // also save UTF8 data as Ansi, UTF8 or Unicode.
  TsdUtf8Stream = class(TsdCodecStream)
  private
  protected
    function ReadByte: byte; override;
    procedure WriteByte(const B: byte); override;
    procedure WriteBuf(const Buffer; Offset, Count: longint); override;
  end;

  // TsdSurplusReader is a simple class that can store a few surplus characters
  // and returns these first before reading from the underlying stream
  TsdSurplusReader = class
  private
    FStream: TStream;
    FSurplus: UTF8String;
  public
    constructor Create(AStream: TStream);
    property Surplus: UTF8String read FSurplus write FSurplus;
    function ReadChar(var Ch: AnsiChar): integer;
    function ReadCharSkipBlanks(var Ch: AnsiChar): boolean;
  end;

  // Simple string builder class that allocates string memory more effectively
  // to avoid repeated re-allocation
  TsdStringBuilder = class
  private
    FData: UTF8String;
    FCurrentIdx: integer;
    function GetData(Index: integer): AnsiChar;
    procedure Reallocate(RequiredLength: integer);
  public
    constructor Create;
    procedure Clear;
    procedure AddChar(Ch: AnsiChar);
    procedure AddString(var S: UTF8String);
    function StringCopy(AFirst, ALength: integer): UTF8String;
    function Value: UTF8String;
    property Length: integer read FCurrentIdx;
    property Data[Index: integer]: AnsiChar read GetData; default;
  end;

// String functions

function sdUTF8StringReplace(const S, OldPattern, NewPattern: UTF8String): UTF8String;

// Escape all required characters in string AValue.
function sdUTF8EscapeString(const AValue: UTF8String): UTF8String;

// Replace all escaped characters in string AValue by their original. This includes
// character references using &#...; and &#x...;
function sdUTF8UnEscapeString(const AValue: UTF8String): UTF8String;

// Enclose the string AValue in quotes.
function sdUTF8QuotedString(const AValue: UTF8String): UTF8String;

// Remove the quotes from string AValue.
function sdUTF8UnQuotedString(const AValue: UTF8String): UTF8String;

// This function adds control characters Chars repeatedly after each Interval
// of characters to UTF8String Value.
function sdAddControlChars(const AValue: UTF8String; const Chars: UTF8String; Interval: integer): UTF8String;

// This function removes control characters from UTF8String AValue (Tab, CR, LF and Space)
function sdRemoveControlChars(const AValue: UTF8String): UTF8String;

// Convert the UTF8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, an exception will be raised.
function sdDateTimeFromString(const ADate: UTF8String; UseLocalBias: Boolean = False): TDateTime;

// Convert the UTF8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
function sdDateTimeFromStringDefault(const ADate: UTF8String; ADefault: TDateTime;
  UseLocalBias: Boolean = False): TDateTime;

// Convert the TDateTime ADate to a UTF8String according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
function sdDateTimeToString(ADate: TDateTime; UseLocalBias: Boolean = False): UTF8String;

// Convert a number to a UTF8String, using SignificantDigits to indicate the number of
// significant digits, and AllowScientific to allow for scientific notation if that
// results in much shorter notatoin.
function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): UTF8String;

// Write an UTF8String to the stream
procedure sdUTF8WriteStringToStream(S: TStream; const AString: UTF8String);

// Turn Ch to an UpCase character ('a'..'z' becomes 'A'..'Z')
function sdUpCase(Ch: AnsiChar): AnsiChar;

// Conversion between Ansi, UTF8 and Unicode

// Convert a UnicodeString to a UTF8 encoded string
function sdUnicodeToUtf8(const W: UnicodeString): UTF8String;

// Convert a AnsiString to a UTF8 encoded string
function sdAnsiToUtf8(const S: AnsiString): UTF8String;

// Convert a UTF8 encoded string to a UnicodeString
function sdUtf8ToUnicode(const S: UTF8String): UnicodeString;

// Convert a UTF8 encoded string to a AnsiString. This might cause loss!
function sdUtf8ToAnsi(const S: UTF8String): AnsiString;


// parse functions

// Find SubString within UTF8String S, only process characters between Start and Close.
// First occurrance is reported in APos. If something is found, function returns True.
function sdUTF8FindString(const SubString, S: UTF8String; Start, Close: integer; var APos: integer): boolean;

// Detect if the SubString matches the characters in S from position Start. S may be
// actually longer than SubString, only length(SubString) characters are checked.
function sdUTF8MatchString(const SubString: UTF8String; const S: UTF8String; Start: integer): boolean;

// Find all Name="Value" pairs in UTF8String AValue (from Start to Close - 1), and put
// the resulting attributes in stringlist Attributes. This stringlist must already
// be initialized when calling this function.
procedure sdUTF8ParseAttributes(const AValue: UTF8String; Start, Close: integer; Attributes: TsdUTF8StringList);

// Trim the UTF8String AValue between Start and Close - 1 (remove whitespaces at start
// and end), not by adapting the UTF8String but by adjusting the Start and Close indices.
// If the resulting UTF8String still has a length > 0, the function returns True.
function sdUTF8TrimPos(const AValue: UTF8String; var Start, Close: integer): boolean;

// Trim the UTF8String
function sdUTF8Trim(const AValue: UTF8String): UTF8String;

// Encoding/Decoding functions

// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as UTF8String, without any linebreaks.
function EncodeBase64(const Source: RawByteString): UTF8String;

// Decode BASE64 data in Source into binary data. The function returns the binary
// data as UTF8String. Use a TStringStream to convert this data to a stream. The Source
// UTF8String may contain linebreaks and control characters, these will be stripped.
function DecodeBase64(const Source: UTF8String): RawByteString;

// Encode binary data in Source as BINHEX. The function returns the BINHEX encoded
// data as UTF8String, without any linebreaks.
function EncodeBinHex(const Source: RawByteString): UTF8String;

// Decode BINHEX data in Source into binary data. The function returns the binary
// data as UTF8String. Use a TStringStream to convert this data to a stream. The Source
// UTF8String may contain linebreaks and control characters, these will be stripped.
function DecodeBinHex(const Source: UTF8String): RawByteString;

resourcestring

  sxeErrorCalcStreamLength       = 'Error while calculating streamlength';
  sxeMissingDataInBinaryStream   = 'Missing data in binary stream';
  sxeMissingElementName          = 'Missing element name';
  sxeMissingCloseTag             = 'Missing close tag in element %s';
  sxeMissingDataAfterGreaterThan = 'Missing data after "<" in element %s';
  sxeMissingLessThanInCloseTag   = 'Missing ">" in close tag of element %s';
  sxeIncorrectCloseTag           = 'Incorrect close tag in element %s';
  sxeIllegalCharInNodeName       = 'Illegal character in node name "%s"';
  sxeMoreThanOneRootElement      = 'More than one root element found in xml';
  sxeMoreThanOneDeclaration      = 'More than one xml declaration found in xml';
  sxeDeclarationMustBeFirstElem  = 'Xml declaration must be first element';
  sxeMoreThanOneDoctype          = 'More than one doctype declaration found in root';
  sxeDoctypeAfterRootElement     = 'Doctype declaration found after root element';
  sxeNoRootElement               = 'No root element found in xml';
  sxeIllegalElementType          = 'Illegal element type';
  sxeCDATAInRoot                 = 'No CDATA allowed in root';
  sxeRootElementNotDefined       = 'XML root element not defined.';
  sxeCodecStreamNotAssigned      = 'Encoding stream unassigned';
  sxeUnsupportedEncoding         = 'Unsupported string encoding';
  sxeCannotReadCodecForWriting   = 'Cannot read from a conversion stream opened for writing';
  sxeCannotWriteCodecForReading  = 'Cannot write to an UTF stream opened for reading';
  sxeCannotReadMultipeChar       = 'Cannot read multiple chars from conversion stream at once';
  sxeCannotPerformSeek           = 'Cannot perform seek on codec stream';
  sxeCannotSeekBeforeReadWrite   = 'Cannot seek before reading or writing in conversion stream';
  sxeCannotSeek                  = 'Cannot perform seek in conversion stream';
  sxeCannotWriteToOutputStream   = 'Cannot write to output stream';
  sxeXmlNodeNotAssigned          = 'XML Node is not assigned';
  sxeCannotConverToBool          = 'Cannot convert value to bool';
  sxeCannotConvertToFloat        = 'Cannot convert value to float';
  sxeSignificantDigitsOutOfRange = 'Significant digits out of range';

implementation

type

  // Internal type
  TTagType = record
    Start: UTF8String;
    Close: UTF8String;
    Style: TXmlElementType;
  end;
  PByte = ^byte;

  TBomInfo = packed record
    BOM: array[0..3] of byte;
    Len: integer;
    Encoding: TStringEncodingType;
    HasBOM: boolean;
  end;

const

  // Count of different escape characters
  cEscapeCount = 5;

  // These are characters that must be escaped. Note that "&" is first since
  // when another would be replaced first (eg ">" by "&lt;") this could
  // cause the new "&" in "&lt;" to be replaced by "&amp;";
  cEscapes: array[0..cEscapeCount - 1] of UTF8String =
    ('&', '<', '>', '''', '"');

  // These are the strings that replace the escape strings - in the same order
  cReplaces: array[0..cEscapeCount - 1] of UTF8String =
    ('&amp;', '&lt;', '&gt;', '&apos;', '&quot;');

  cQuoteChars: set of AnsiChar = ['"', ''''];
  cControlChars: set of AnsiChar = [#9, #10, #13, #32]; {Tab, LF, CR, Space}

  // Count of different XML tags
  cTagCount = 12;

  cTags: array[0..cTagCount - 1] of TTagType = (
    // The order is important here; the items are searched for in appearing order
    (Start: '<![CDATA[';        Close: ']]>'; Style: xeCData),
    (Start: '<!DOCTYPE';        Close: '>';   Style: xeDoctype),
    (Start: '<!ELEMENT';        Close: '>';   Style: xeElement),
    (Start: '<!ATTLIST';        Close: '>';   Style: xeAttList),
    (Start: '<!ENTITY';         Close: '>';   Style: xeEntity),
    (Start: '<!NOTATION';       Close: '>';   Style: xeNotation),
    (Start: '<?xml-stylesheet'; Close: '?>';  Style: xeStylesheet),
    (Start: '<?xml';            Close: '?>';  Style: xeDeclaration),
    (Start: '<!--';             Close: '-->'; Style: xeComment),
    (Start: '<!';               Close: '>';   Style: xeExclam),
    (Start: '<?';               Close: '?>';  Style: xeQuestion),
    (Start: '<';                Close: '>';   Style: xeNormal) );
    // direct tags are derived from Normal tags by checking for the />

  // These constant are used when generating hexchars from buffer data
  cHexChar:       array[0..15] of AnsiChar = '0123456789ABCDEF';
  cHexCharLoCase: array[0..15] of AnsiChar = '0123456789abcdef';

  // These AnsiCharacters are used when generating BASE64 AnsiChars from buffer data
  cBase64Char: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  cBase64PadChar: AnsiChar = '=';

  // The amount of bytes to allocate with each increase of the value buffer
  cNodeValueBuf = 2048;

  // byte order marks for strings
  // Unicode text files should contain $FFFE as first character to identify such a file clearly. Depending on the system
  // where the file was created on this appears either in big endian or little endian style.

  const cBomInfoCount = 15;
  const cBomInfo: array[0..cBomInfoCount - 1] of TBomInfo =
  ( (BOM: ($00,$00,$FE,$FF); Len: 4; Encoding: seUCS4BE;    HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 4; Encoding: seUCS4LE;    HasBOM: true),
    (BOM: ($00,$00,$FF,$FE); Len: 4; Encoding: seUCS4_2143; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 2; Encoding: seUTF16BE;   HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 2; Encoding: seUTF16LE;   HasBOM: true),
    (BOM: ($EF,$BB,$BF,$00); Len: 3; Encoding: seUTF8;      HasBOM: true),
    (BOM: ($00,$00,$00,$3C); Len: 4; Encoding: seUCS4BE;    HasBOM: false),
    (BOM: ($3C,$00,$00,$00); Len: 4; Encoding: seUCS4LE;    HasBOM: false),
    (BOM: ($00,$00,$3C,$00); Len: 4; Encoding: seUCS4_2143; HasBOM: false),
    (BOM: ($00,$3C,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: false),
    (BOM: ($00,$3C,$00,$3F); Len: 4; Encoding: seUTF16BE;   HasBOM: false),
    (BOM: ($3C,$00,$3F,$00); Len: 4; Encoding: seUTF16LE;   HasBOM: false),
    (BOM: ($3C,$3F,$78,$6D); Len: 4; Encoding: seAnsi;      HasBOM: false),
    (BOM: ($4C,$6F,$A7,$94); Len: 4; Encoding: seEBCDIC;    HasBOM: false)
  );

function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function UTF8QuotedStr(const S: UTF8String; Quote: AnsiChar): UTF8String;
var
  P, Src, Dest: PAnsiChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := StrScan(PAnsiChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := UTF8String(Quote) + S + UTF8String(Quote);
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := StrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := StrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function UTF8ExtractQuotedStr(const S: UTF8String; Quote: AnsiChar): UTF8String;
var
  P, Src, Dest: PAnsiChar;
  DropCount: Integer;
begin
  Result := '';
  Src := PAnsiChar(S);
  if (Src = nil) or (Src^ <> Quote) then
    Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScan(Src, Quote);
  while Src <> nil do
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScan(Src, Quote);
  end;
  if Src = nil then
    Src := StrEnd(P);
  if ((Src - P) <= 1) then
    Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PAnsiChar(Result);
    Src := StrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScan(Src, Quote);
    end;
    if Src = nil then
      Src := StrEnd(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function Utf8Pos(const Substr, S: UTF8String): Integer;
var
  i, x: Integer;
  Len, LenSubStr: Integer;
begin
  i := 1;
  LenSubStr := Length(SubStr);
  Len := Length(S) - LenSubStr + 1;
  while i <= Len do
  begin
    if S[i] = SubStr[1] then
    begin
      x := 1;
      while (x < LenSubStr) and (S[i + x] = SubStr[x + 1]) do
        Inc(x);
      if (x = LenSubStr) then
      begin
        Result := i;
        exit;
      end;
    end;
    Inc(i);
  end;
  Result := 0;
end;

// TStream.Write

function StreamWrite(Stream: TStream; const Buffer; Offset, Count: Longint): Longint;
begin
  Result := Stream.Write(TBigByteArray(Buffer)[Offset], Count);
end;

// Delphi's implementation of TStringStream is severely flawed, it does a SetLength
// on each write, which slows down everything to a crawl. This implementation over-
// comes this issue.
type
  TsdUTF8StringStream = class(TMemoryStream)
  public
    constructor Create(const S: UTF8String);
    function DataString: UTF8String;
  end;

constructor TsdUTF8StringStream.Create(const S: UTF8String);
begin
  inherited Create;
  SetSize(length(S));
  if Size > 0 then
  begin
    Write(S[1], Size);
    Position := 0;
  end;
end;

function TsdUTF8StringStream.DataString: UTF8String;
begin
  SetLength(Result, Size);
  if Size > 0 then
  begin
    Position := 0;
    Read(Result[1], length(Result));
  end;
end;

// Utility functions

function Min(A, B: integer): integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: integer): integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function sdUTF8StringReplace(const S, OldPattern, NewPattern: UTF8String): UTF8String;
var
  SearchStr, NewStr: UTF8String;
  Offset: Integer;
begin
  // Case Sensitive, Replace All
  SearchStr := S;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := UTF8Pos(OldPattern, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    SearchStr := Copy(SearchStr, Offset + Length(OldPattern), MaxInt);
  end;
end;

function sdUTF8EscapeString(const AValue: UTF8String): UTF8String;
var
  i: integer;
begin
  Result := AValue;
  for i := 0 to cEscapeCount - 1 do
    Result := sdUTF8StringReplace(Result, cEscapes[i], cReplaces[i]);
end;

function sdUTF8UnEscapeString(const AValue: UTF8String): UTF8String;
var
  SearchStr, Reference, Replace: UTF8String;
  i, Offset, Code: Integer;
  W: word;
begin
  SearchStr := AValue;
  Result := '';
  while SearchStr <> '' do
  begin
    // find '&'
    Offset := Utf8Pos('&', SearchStr);
    if Offset = 0 then
    begin
      // Nothing found
      Result := Result + SearchStr;
      Break;
    end;
    Result := Result + Copy(SearchStr, 1, Offset - 1);
    SearchStr := Copy(SearchStr, Offset, MaxInt);
    // find next ';'
    Offset := Utf8Pos(';', SearchStr);
    if Offset = 0 then
    begin
      // Error: encountered a '&' but not a ';'.. we will ignore, just return
      // the unmodified value
      Result := Result + SearchStr;
      Break;
    end;
    // Reference
    Reference := copy(SearchStr, 1, Offset);
    SearchStr := Copy(SearchStr, Offset + 1, MaxInt);
    Replace := Reference;
    // See if it is a Character reference
    if copy(Reference, 1, 2) = '&#' then
    begin
      Reference := copy(Reference, 3, length(Reference) - 3);
      if length(Reference) > 0 then
      begin
        if sdUpCase(Reference[1]) = 'X' then
          // Hex notation
          Reference[1] := '$';
        Code := StrToIntDef(string(Reference), -1);
        if (Code >= 0) and (Code < $FFFF) then
        begin
          W := Code;
          Replace := sdUnicodeToUtf8(UnicodeChar(W));
        end;
      end;
    end else
    begin
      // Look up default escapes
      for i := 0 to cEscapeCount - 1 do
        if Reference = cReplaces[i] then
        begin
          // Replace
          Replace := cEscapes[i];
          Break;
        end;
    end;
    // New result
    Result := Result + Replace;
  end;
end;

function sdUTF8QuotedString(const AValue: UTF8String): UTF8String;
var
  Quote: AnsiChar;
begin
  Quote := '"';
  if UTF8Pos('"', AValue) > 0 then
    Quote := '''';
  Result := UTF8QuotedStr(AValue, Quote);
end;

function sdUTF8UnQuotedString(const AValue: UTF8String): UTF8String;
var
  Quote: AnsiChar;
begin
  if Length(AValue) < 2 then
  begin
    Result := AValue;
    exit;
  end;
  Quote := AValue[1];
  if Quote in cQuoteChars then
  begin
    Result := UTF8ExtractQuotedStr(AValue, Quote);
  end else
    Result := AValue;
end;

function sdAddControlChars(const AValue: UTF8String; const Chars: UTF8String; Interval: integer): UTF8String;
// Insert AnsiChars in AValue at each Interval AnsiChars
var
  i, j, ALength: integer;
  // local
  procedure InsertControlChars;
  var
    k: integer;
  begin
    for k := 1 to Length(Chars) do
    begin
      Result[j] := Chars[k];
      inc(j);
    end;
  end;
// main
begin
  if (Length(Chars) = 0) or (Interval <= 0) then
  begin
    Result := AValue;
    exit;
  end;

  // Calculate length based on original length and total extra length for control AnsiChars
  ALength := Length(AValue) + ((Length(AValue) - 1) div Interval + 3) * Length(Chars);
  SetLength(Result, ALength);

  // Copy and insert
  j := 1;
  for i := 1 to Length(AValue) do
  begin
    if (i mod Interval) = 1 then
      // Insert control AnsiChars
      InsertControlChars;
    Result[j] := AValue[i];
    inc(j);
  end;
  InsertControlChars;

  // Adjust length
  dec(j);
  if ALength > j then
    SetLength(Result, j);
end;

function sdRemoveControlChars(const AValue: UTF8String): UTF8String;
// Remove control characters from UTF8String in AValue
var
  i, j: integer;
begin
  Setlength(Result, Length(AValue));
  i := 1;
  j := 1;
  while i <= Length(AValue) do
    if AValue[i] in cControlChars then
      inc(i)
    else
    begin
      Result[j] := AValue[i];
      inc(i);
      inc(j);
    end;
  // Adjust length
  if i <> j then
    SetLength(Result, j - 1);
end;

function sdUTF8FindString(const SubString, S: UTF8String; Start, Close: integer; var APos: integer): boolean;
// Check if the Substring matches the UTF8String S in any position in interval Start to Close - 1
// and returns found positon in APos. Result = True if anything is found.
// Note: this funtion is case-insensitive
var
  CharIndex: integer;
begin
  Result := False;
  APos := 0;
  for CharIndex := Start to Close - Length(SubString) do
    if sdUTF8MatchString(SubString, S, CharIndex) then
    begin
      APos := CharIndex;
      Result := True;
      exit;
    end;
end;

function UTF8CompareText(const S1, S2: UTF8String): integer;
begin
  Result := AnsiCompareText(string(S1), string(S2));
end;

function IntToUTF8Str(Value: integer): UTF8String;
begin
  Result := UTF8String(IntToStr(Value));
end;

function Int64ToUTF8Str(Value: int64): UTF8String;
begin
  Result := UTF8String(IntToStr(Value));
end;

function sdUTF8MatchString(const SubString: UTF8String; const S: UTF8String; Start: integer): boolean;
// Check if the Substring matches the string S at position Start.
// Note: this funtion is case-insensitive
var
  CharIndex: integer;
begin
  Result := False;
  // Check range just in case
  if (Length(S) - Start + 1) < Length(Substring) then
    exit;

  CharIndex := 0;
  while CharIndex < Length(SubString) do
    if sdUpCase(SubString[CharIndex + 1]) = sdUpCase(S[Start + CharIndex]) then
      inc(CharIndex)
    else
      exit;
  // All AnsiChars were the same, so we succeeded
  Result := True;
end;

procedure sdUTF8ParseAttributes(const AValue: UTF8String; Start, Close: integer; Attributes: TsdUTF8StringList);
// Convert the attributes string AValue in [Start, Close - 1] to the attributes Stringlist
var
  i: integer;
  InQuotes: boolean;
  Quote: AnsiChar;
begin
  InQuotes := False;
  Quote := '"';
  if not assigned(Attributes) then
    exit;
  if not sdUTF8TrimPos(AValue, Start, Close) then
    exit;

  // Clear first
  Attributes.Clear;

  // Loop through characters
  for i := Start to Close - 1 do
  begin

    // In quotes?
    if InQuotes then
    begin
      if AValue[i] = Quote then
        InQuotes := False;
    end else
    begin
      if AValue[i] in cQuoteChars then
      begin
        InQuotes   := True;
        Quote := AValue[i];
      end;
    end;

    // Add attribute strings on each controlchar break
    if not InQuotes then
      if AValue[i] in cControlChars then
      begin
        if i > Start then
          Attributes.Add(copy(AValue, Start, i - Start));
        Start := i + 1;
      end;
  end;

  // Add last attribute string
  if Start < Close then
    Attributes.Add(copy(AValue, Start, Close - Start));

  // First-char "=" signs should append to previous
  for i := Attributes.Count - 1 downto 1 do
    if Attributes[i][1] = '=' then
    begin
      Attributes[i - 1] := Attributes[i - 1] + Attributes[i];
      Attributes.Delete(i);
    end;

  // First-char quotes should append to previous
  for i := Attributes.Count - 1 downto 1 do
    if (Attributes[i][1] in cQuoteChars) and (UTF8Pos('=', Attributes[i - 1]) > 0) then
    begin
      Attributes[i - 1] := Attributes[i - 1] + Attributes[i];
      Attributes.Delete(i);
    end;
end;

function sdUTF8TrimPos(const AValue: UTF8String; var Start, Close: integer): boolean;
// Trim the string in AValue in [Start, Close - 1] by adjusting Start and Close variables
begin
  // Checks
  Start := Max(1, Start);
  Close := Min(Length(AValue) + 1, Close);
  if Close <= Start then
  begin
    Result := False;
    exit;
  end;

  // Trim left
  while
    (Start < Close) and
    (AValue[Start] in cControlChars) do
    inc(Start);

  // Trim right
  while
    (Start < Close) and
    (AValue[Close - 1] in cControlChars) do
    dec(Close);

  // Do we have a string left?
  Result := Close > Start;
end;

function sdUTF8Trim(const AValue: UTF8String): UTF8String;
var
  Start, Close: integer;
  Res: boolean;
begin
  Start := 1;
  Close := length(AValue) + 1;
  Res := sdUTF8TrimPos(AValue, Start, Close);
  if Res then
    Result := Copy(AValue, Start, Close - Start)
  else
    Result := '';
end;

procedure sdUTF8WriteStringToStream(S: TStream; const AString: UTF8String);
begin
  if Length(AString) > 0 then
  begin
    S.Write(AString[1], Length(AString));
  end;
end;

function sdUpCase(Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  case Result of
  'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;

function ReadOpenTag(AReader: TsdSurplusReader): integer;
// Try to read the type of open tag from S
var
  AIndex, i: integer;
  Found: boolean;
  Ch: AnsiChar;
  Candidates: array[0..cTagCount - 1] of boolean;
  Surplus: UTF8String;
begin
  Surplus := '';
  Result := cTagCount - 1;
  for i := 0 to cTagCount - 1 do Candidates[i] := True;
  AIndex := 1;
  repeat
    Found := False;
    inc(AIndex);
    if AReader.ReadChar(Ch) = 0 then
      exit;
    Surplus := Surplus + UTF8String(Ch);
    for i := cTagCount - 1 downto 0 do
      if Candidates[i] and (length(cTags[i].Start) >= AIndex) then
      begin
        if cTags[i].Start[AIndex] = Ch then
        begin
          Found := True;
          if length(cTags[i].Start) = AIndex then
            Result := i;
        end else
          Candidates[i] := False;
      end;
  until Found = False;
  // The surplus string that we already read (everything after the tag)
  AReader.Surplus := copy(Surplus, length(cTags[Result].Start), length(Surplus));
end;

function ReadStringFromStreamUntil(AReader: TsdSurplusReader; const ASearch: UTF8String;
  var AValue: UTF8String; SkipQuotes: boolean): boolean;
var
  AIndex, ValueIndex, SearchIndex: integer;
  LastSearchChar, Ch: AnsiChar;
  InQuotes: boolean;
  QuoteChar: AnsiChar;
  SB: TsdStringBuilder;
begin
  Result := False;
  InQuotes := False;

  // Get last searchstring character
  AIndex := length(ASearch);
  if AIndex = 0 then exit;
  LastSearchChar := ASearch[AIndex];

  SB := TsdStringBuilder.Create;
  try
    QuoteChar := #0;

    repeat
      // Add characters to the value to be returned
      if AReader.ReadChar(Ch) = 0 then
        exit;
      SB.AddChar(Ch);

      // Do we skip quotes?
      if SkipQuotes then
      begin
        if InQuotes then
        begin
          if (Ch = QuoteChar) then
            InQuotes := false;
        end else
        begin
          if Ch in cQuoteChars then
          begin
            InQuotes := true;
            QuoteChar := Ch;
          end;
        end;
      end;

      // In quotes? If so, we don't check the end condition
      if not InQuotes then
      begin
        // Is the last char the same as the last char of the search string?
        if Ch = LastSearchChar then
        begin

          // Check to see if the whole search string is present
          ValueIndex  := SB.Length - 1;
          SearchIndex := length(ASearch) - 1;
          if ValueIndex < SearchIndex then continue;

          Result := True;
          while (SearchIndex > 0)and Result do
          begin
            Result := SB[ValueIndex] = ASearch[SearchIndex];
            dec(ValueIndex);
            dec(SearchIndex);
          end;
        end;
      end;
    until Result;

    // Use only the part before the search string
    AValue := SB.StringCopy(1, SB.Length - length(ASearch));
  finally
    SB.Free;
  end;
end;

function ReadStringFromStreamWithQuotes(S: TStream; const Terminator: UTF8String;
  var AValue: UTF8String): boolean;
var
  Ch, QuoteChar: AnsiChar;
  InQuotes: boolean;
  SB: TsdStringBuilder;
begin
  SB := TsdStringBuilder.Create;
  try
    QuoteChar := #0;
    Result := False;
    InQuotes := False;
    repeat
      if S.Read(Ch, 1) = 0 then exit;
      if not InQuotes then
      begin
        if (Ch = '"') or (Ch = '''') then
        begin
          InQuotes := True;
          QuoteChar := Ch;
        end;
      end else
      begin
        if Ch = QuoteChar then
          InQuotes := False;
      end;
      if not InQuotes and (UTF8String(Ch) = Terminator) then
        break;
      SB.AddChar(Ch);
    until False;
    AValue := SB.Value;
    Result := True;
  finally
    SB.Free;
  end;
end;

function GetTimeZoneBias: Integer;
// uses windows unit, func GetTimeZoneInformation
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_UNKNOWN: Result := TimeZoneInfo.Bias;
    TIME_ZONE_ID_STANDARD: Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
  else
    Result := 0;
  end;
end;

function sdDateTimeFromString(const ADate: UTF8String; UseLocalBias: Boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// contributor: Stefan Glienke
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
  ALocalBias, ABias: Integer;
begin
  AYear  := StrToInt(string(copy(ADate, 1, 4)));
  AMonth := StrToInt(string(copy(ADate, 6, 2)));
  ADay   := StrToInt(string(copy(ADate, 9, 2)));
  if Length(ADate) > 16 then
  begin
    AHour := StrToInt(string(copy(ADate, 12, 2)));
    AMin  := StrToInt(string(copy(ADate, 15, 2)));
    ASec  := StrToIntDef(string(copy(ADate, 18, 2)), 0); // They might be omitted, so default to 0
    AMSec := StrToIntDef(string(copy(ADate, 21, 3)), 0); // They might be omitted, so default to 0
  end else
  begin
    AHour := 0;
    AMin  := 0;
    ASec  := 0;
    AMSec := 0;
  end;
  Result :=
    EncodeDate(AYear, AMonth, ADay) +
    EncodeTime(AHour, AMin, ASec, AMSec);
  ALocalBias := GetTimeZoneBias;
  if UseLocalBias then
  begin
    if (Length(ADate) > 24) then
    begin
      ABias := StrToInt(string(Copy(ADate, 25, 2))) * MinsPerHour +
        StrToInt(string(Copy(ADate, 28, 2)));
      if ADate[24] = '+' then
        ABias := ABias * -1;
      Result := Result + ABias / MinsPerDay;
    end;
    Result := Result - ALocalBias / MinsPerDay;
  end;
end;

function sdDateTimeFromStringDefault(const ADate: UTF8String; ADefault: TDateTime; UseLocalBias: Boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
begin
  try
    Result := sdDateTimeFromString(ADate, UseLocalBias);
  except
    Result := ADefault;
  end;
end;

function sdDateTimeToString(ADate: TDateTime; UseLocalBias: Boolean): UTF8String;
// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// contributor: Stefan Glienke
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
  ABias: Integer;
const
  Neg: array[Boolean] of string = ('+', '-');
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  DecodeTime(ADate, AHour, AMin, ASec, AMSec);
  if frac(ADate) = 0 then
    Result := UTF8String(Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]))
  else
  begin
    ABias := GetTimeZoneBias;
    if UseLocalBias and (ABias <> 0) then
      Result := UTF8String(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3d%s%.2d:%.2d',
        [AYear, AMonth, ADay, AHour, AMin, ASec, AMSec,
        Neg[ABias > 0], Abs(ABias) div MinsPerHour, Abs(ABias) mod MinsPerHour]))
    else
      Result := UTF8String(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
        [AYear, AMonth, ADay, AHour, AMin, ASec, AMSec]));
  end;
end;

function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): UTF8String;
const
  Limits: array[1..9] of integer =
    (10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  Limit, Limitd, PointPos, IntVal, ScPower: integer;
  Body: UTF8String;
begin
  if (SignificantDigits < 1) or (SignificantDigits > 9) then
    raise Exception.Create(sxeSignificantDigitsOutOfRange);

  // Zero
  if Value = 0 then
  begin
    Result := '0';
    exit;
  end;

  // Sign
  if Value < 0 then
  begin
    Result := '-';
    Value := -Value;
  end else
    Result := '';

  // Determine point position
  Limit := Limits[SignificantDigits];
  Limitd := Limit div 10;
  PointPos := SignificantDigits;
  while Value < Limitd do
  begin
    Value := Value * 10;
    dec(PointPos);
  end;
  while Value >= Limit do
  begin
    Value := Value * 0.1;
    inc(PointPos);
  end;

  // Round
  IntVal := round(Value);

  // Exceptional case which happens when the value rounds up to the limit
  if Intval = Limit then
  begin
    IntVal := IntVal div 10;
    inc(PointPos);
  end;

  // Strip off any zeros, these reduce significance count
  while (IntVal mod 10 = 0) and (PointPos < SignificantDigits) do
  begin
    dec(SignificantDigits);
    IntVal := IntVal div 10;
  end;

  // Check for scientific notation
  ScPower := 0;
  if AllowScientific and ((PointPos < -1) or (PointPos > SignificantDigits + 2)) then
  begin
    ScPower := PointPos - 1;
    dec(PointPos, ScPower);
  end;

  // Body
  Body := IntToUTF8Str(IntVal);
  while PointPos > SignificantDigits do
  begin
    Body := Body + '0';
    inc(SignificantDigits);
  end;
  while PointPos < 0 do
  begin
    Body := '0' + Body;
    inc(PointPos);
  end;
  if PointPos = 0 then
    Body := '.' + Body
  else
    if PointPos < SignificantDigits then
      Body := copy(Body, 1, PointPos) + '.' + copy(Body, PointPos + 1, SignificantDigits);

  // Final result
  if ScPower = 0 then
    Result := Result + Body
  else
    Result := Result + Body + 'E' + IntToUTF8Str(ScPower);
end;


function PtrUnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PUnicodeChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if not assigned(Source) or not assigned(Dest) then
    exit;

  count := 0;
  i := 0;

  while (i < SourceChars) and (count < MaxDestBytes) do
  begin
    c := Cardinal(Source[i]);
    Inc(i);
    if c <= $7F then
    begin
      Dest[count] := AnsiChar(c);
      Inc(count);
    end else
      if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($E0 or (c shr 12));
        Dest[count+1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[count+2] := AnsiChar($80 or (c and $3F));
        Inc(count,3);
      end else
      begin //  $7F < Source[i] <= $7FF
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($C0 or (c shr 6));
        Dest[count+1] := AnsiChar($80 or (c and $3F));
        Inc(count,2);
      end;
  end;
  if count >= MaxDestBytes then
    count := MaxDestBytes-1;
  Dest[count] := #0;
  Result := count + 1;  // convert zero based index to byte count
end;

function PtrUtf8ToUnicode(Dest: PUnicodeChar; MaxDestChars: Cardinal; Source: PAnsiChar;
  SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if not assigned(Dest) or not assigned(Source) then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  while (i < SourceBytes) and (count < MaxDestChars) do
  begin
    wc := Cardinal(Source[i]);
    Inc(i);
    if (wc and $80) <> 0 then
    begin
      if i >= SourceBytes then
        // incomplete multibyte char
        Exit;
      wc := wc and $3F;
      if (wc and $20) <> 0 then
      begin
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          // malformed trail byte or out of range char
          Exit;
        if i >= SourceBytes then
          // incomplete multibyte char
          Exit;
        wc := (wc shl 6) or (c and $3F);
      end;
      c := Byte(Source[i]);
      Inc(i);
      if (c and $C0) <> $80 then
        // malformed trail byte
        Exit;
      Dest[count] := UnicodeChar((wc shl 6) or (c and $3F));
    end else
      Dest[count] := UnicodeChar(wc);
    Inc(count);
  end;

  if count >= MaxDestChars then
    count := MaxDestChars-1;

  Dest[count] := #0;
  Result := count + 1;
end;

function sdUnicodeToUtf8(const W: UnicodeString): UTF8String;
var
  L: integer;
  Temp: UTF8String;
begin
  Result := '';
  if W = '' then
    Exit;
  SetLength(Temp, Length(W) * 3); // SetLength includes space for null terminator

  L := PtrUnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PUnicodeChar(W), Length(W));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function sdUtf8ToUnicode(const S: UTF8String): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));

  L := PtrUtf8ToUnicode(PUnicodeChar(Temp), Length(Temp)+1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function EncodeBase64Buf(const Buffer; Count: Integer): UTF8String;
var
  i, j: integer;
  ACore: integer;
  ALong: cardinal;
  S: PByte;
begin
  // Make sure ASize is always a multiple of 3, and this multiple
  // gets saved as 4 characters
  ACore := (Count + 2) div 3;

  // Set the length of the string that stores encoded characters
  SetLength(Result, ACore * 4);
  S := @Buffer;
  // Do the loop ACore times
  for i := 0 to ACore - 1 do
  begin
    ALong := 0;
    for j := 0 to 2 do
    begin
      ALong := ALong shl 8 + S^;
      inc(S);
    end;
    for j := 0 to 3 do
    begin
      Result[i * 4 + 4 - j] := cBase64Char[ALong and $3F];
      ALong := ALong shr 6;
    end;
  end;
  // For comformity to Base64, we must pad the data instead of zero out
  // if the size is not an exact multiple of 3
  case ACore * 3 - Count of
  0:;// nothing to do
  1: // pad one byte
    Result[ACore * 4] := cBase64PadChar;
  2: // pad two bytes
    begin
      Result[ACore * 4    ] := cBase64PadChar;
      Result[ACore * 4 - 1] := cBase64PadChar;
    end;
  end;//case
end;

function EncodeBase64(const Source: RawByteString): UTF8String;
// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as string, without any linebreaks.
begin
  if length(Source) > 0 then
    Result := EncodeBase64Buf(Source[1], length(Source))
  else
    Result := '';
end;

procedure DecodeBase64Buf(var Source: UTF8String; var Buffer; Count: Integer);
var
  i, j: integer;
  BufPos, Core: integer;
  LongVal: cardinal;
  D: PByte;
  Map: array[AnsiChar] of byte;
begin
  // Core * 4 is the number of chars to read - check length
  Core := Length(Source) div 4;
  if Count > Core * 3 then
    raise EFilerError.Create(sxeMissingDataInBinaryStream);

  // Prepare map
  for i := 0 to 63 do
    Map[cBase64Char[i]] := i;
  D := @Buffer;

  // Check for final padding, and replace with "zeros". There can be
  // at max two pad chars ('=')
  BufPos := length(Source);
  if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
  begin
    Source[BufPos] := cBase64Char[0];
    dec(BufPos);
    if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
      Source[BufPos] := cBase64Char[0];
  end;

  // Do this Core times
  for i := 0 to Core - 1 do
  begin
    LongVal := 0;
    // Unroll the characters
    for j := 0 to 3 do
      LongVal := LongVal shl 6 + Map[Source[i * 4 + j + 1]];
    // and unroll the bytes
    for j := 2 downto 0 do
    begin
      // Check overshoot
      if integer(D) - integer(@Buffer) >= Count then
        exit;
      D^ := LongVal shr (j * 8) and $FF;
      inc(D);
    end;
  end;
end;

function DecodeBase64(const Source: UTF8String): RawByteString;
// Decode BASE64 data in Source into binary data. The function returns the binary
// data as UTF8String. Use a TStringStream to convert this data to a stream.
var
  BufData: UTF8String;
  BufSize, BufPos: integer;
begin
  BufData := sdRemoveControlChars(Source);

  // Determine length of data
  BufSize := length(BufData) div 4;
  if BufSize * 4 <> length(BufData) then
    raise EFilerError.Create(sxeErrorCalcStreamLength);
  BufSize := BufSize * 3;
  // Check padding AnsiChars
  BufPos := length(BufData);
  if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
  begin
    dec(BufPos);
    dec(BufSize);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      dec(BufSize);
  end;
  Setlength(Result, BufSize);

  // Decode
  if BufSize > 0 then
    DecodeBase64Buf(BufData, Result[1], BufSize);
end;


function sdAnsiToUtf8(const S: AnsiString): UTF8String;
begin
  // We let the OS figure out Ansi<->Unicode
  Result := sdUnicodeToUtf8(UnicodeString(S));
end;

function sdUtf8ToAnsi(const S: UTF8String): AnsiString;
begin
  // We let the OS figure out Ansi<->Unicode. There might be dataloss!
  Result := Ansistring(sdUtf8ToUnicode(S));
end;

function EncodeBinHexBuf(const Source; Count: Integer): UTF8String;
// Encode binary data in Source as BINHEX. The function returns the BINHEX encoded
// data as UTF8String, without any linebreaks.
var
  Text: UTF8String;
begin
  SetLength(Text, Count * 2);
  BinToHex(PAnsiChar(@Source), PAnsiChar(Text), Count);
  Result := Text;
end;

function EncodeBinHex(const Source: RawByteString): UTF8String;
// Encode binary data in Source as BINHEX. The function returns the BINHEX encoded
// data as UTF8String, without any linebreaks.
var
  Text: UTF8String;
begin
  SetLength(Text, Length(Source) * 2);
  BinToHex(PAnsiChar(Source), PAnsiChar(Text), Length(Source));
  Result := Text;
end;

procedure DecodeBinHexBuf(const Source: UTF8String; var Buffer{$IFDEF CLR}: TBytes{$ENDIF}; Count: Integer);
// Decode BINHEX data in Source into binary data.
begin
  if Length(Source) div 2 < Count then
    raise EFilerError.Create(sxeMissingDataInBinaryStream);

  HexToBin(PAnsiChar(Source), PAnsiChar(@Buffer), Count);
end;

function DecodeBinHex(const Source: UTF8String): RawByteString;
// Decode BINHEX data in Source into binary data. The function returns the binary
// data as RawByteString. Use a TStringStream to convert this data to a stream.
var
  Data: Utf8String;
  Size: integer;
  Buffer: RawByteString;
begin
  Data := sdRemoveControlChars(Source);

  // Determine length of data
  Size := length(Data) div 2;
  if Size * 2 <> length(Data) then
    raise EFilerError.Create(sxeErrorCalcStreamLength);

  SetLength(Buffer, Size);
  HexToBin(PAnsiChar(Data), PAnsiChar(Buffer), Size);
  Result := Buffer;
end;

function sdStringToBool(const AValue: UTF8String): boolean;
var
  Ch: AnsiChar;
begin
  if Length(AValue) > 0 then
  begin
    Ch := sdUpCase(AValue[1]);
    if Ch in ['T', 'Y', '1'] then
    begin
      Result := True;
      exit;
    end;
    if Ch in ['F', 'N', '0'] then
    begin
      Result := False;
      exit;
    end;
  end;
  raise Exception.Create(sxeCannotConverToBool);
end;

function sdStringFromBool(ABool: boolean): UTF8String;
const
  cBoolValues: array[boolean] of UTF8String = ('false', 'true');
begin
  Result := cBoolValues[ABool];
end;

{ TsdUTF8StringList }

function TsdUTF8StringList.Add(const S: UTF8String): integer;
var
  L: integer;
begin
  L := Length(FItems);
  if L = FCount then
  begin
    // Increase capacity
    SetLength(FItems, FCount + 4);
  end;
  FItems[FCount] := S;
  Result := FCount;
  inc(FCount);
end;

procedure TsdUTF8StringList.Assign(Source: TPersistent);
var
  i: integer;
  SL: TsdUTF8StringList;
begin
  if Source is TsdUTF8StringList then
  begin
    SL := TsdUTF8StringList(Source);
    SetLength(FItems, SL.FCount);
    for i := 0 to SL.FCount - 1 do
      FItems[i] := SL.FItems[i];
    FCount := SL.FCount;
  end else
    inherited;
end;

procedure TsdUTF8StringList.Clear;
begin
  FCount := 0;
end;

procedure TsdUTF8StringList.Delete(Index: Integer);
var
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then
    exit;
  for i := Index + 1 to Count - 1 do
    FItems[i - 1] := FItems[i];
  dec(FCount);
end;

function TsdUTF8StringList.GetItems(Index: integer): UTF8String;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FItems[Index]
  else
    Result := '';
end;

function TsdUTF8StringList.GetNames(Index: integer): UTF8String;
var
  P: integer;
begin
  Result := Items[Index];
  P := UTF8Pos('=', Result);
  if P <> 0 then
    SetLength(Result, P - 1)
  else
    SetLength(Result, 0);
end;

function TsdUTF8StringList.GetText: UTF8String;
const
  cLB: UTF8String = #13#10;
var
  i, L, LItem: integer;
  P: PAnsiChar;
begin
  L := 0;
  for i := 0 to Count - 1 do
  begin
    inc(L, length(FItems[i]));
    inc(L, 2);
  end;
  SetLength(Result, L);
  if L = 0 then
    exit;
  P := @Result[1];
  for i := 0 to Count - 1 do
  begin
    LItem := length(FItems[i]);
    if LItem > 0 then
    begin
      System.Move(FItems[i][1], P^, LItem);
      inc(P, LItem);
    end;
    System.Move(cLB[1], P^, 2);
    inc(P, 2);
  end;
end;

function TsdUTF8StringList.GetValues(const Name: UTF8String): UTF8String;
var
  Idx: integer;
begin
  Idx := IndexOfName(Name);
  if Idx >= 0 then
    Result := Copy(FItems[Idx], Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

function TsdUTF8StringList.IndexOfName(const Name: UTF8String): integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if sdUTF8MatchString(Name + '=', FItems[Result], 1) then
      exit;
  end;
  Result := -1;
end;

procedure TsdUTF8StringList.SetItems(Index: integer; const Value: UTF8String);
begin
  if (Index >= 0) and (Index < Count) then
    FItems[Index] := Value;
end;

procedure TsdUTF8StringList.SetValues(const Name, Value: UTF8String);
var
  Idx: integer;
begin
  Idx := IndexOfName(Name);
  if Value <> '' then
  begin
    if Idx < 0 then
      Idx := Add('');
    FItems[Idx] := Name + '=' + Value;
  end else
    Delete(Idx);
end;

{ TXmlNode }

function TXmlNode.AbortParsing: boolean;
begin
  Result := assigned(Document) and Document.AbortParsing;
end;

procedure TXmlNode.Assign(Source: TPersistent);
var
  i: integer;
  Node: TXmlNode;
begin
  if Source is TXmlNode then
  begin
    // Clear first
    Clear;

    // Properties
    FElementType := TXmlNode(Source).FElementType;
    FName := TXmlNode(Source).FName;
    FTag := TXmlNode(Source).FTag;
    FValue := TXmlNode(Source).FValue;

    // Attributes
    if assigned(TXmlNode(Source).FAttributes) then
    begin
      CheckCreateAttributesList;
      FAttributes.Assign(TXmlNode(Source).FAttributes);
    end;

    // Nodes
    for i := 0 to TXmlNode(Source).NodeCount - 1 do
    begin
      Node := NodeNew('');
      Node.Assign(TXmlNode(Source).Nodes[i]);
    end;
  end else
    if Source is TRelaxXml then
    begin
      Assign(TRelaxXml(Source).FRootNodes);
    end else
      inherited;
end;

procedure TXmlNode.AttributeAdd(const AName, AValue: UTF8String);
var
  Attr: UTF8String;
begin
  Attr := UTF8String(Format('%s=%s', [AName, sdUTF8QuotedString(sdUTF8EscapeString(AValue))]));
  CheckCreateAttributesList;
  FAttributes.Add(Attr);
end;

procedure TXmlNode.AttributeAdd(const AName: UTF8String; AValue: integer);
begin
  AttributeAdd(AName, IntToUTF8Str(AValue));
end;

procedure TXmlNode.AttributeDelete(Index: integer);
begin
  if (Index >= 0) and (Index < AttributeCount) then
    FAttributes.Delete(Index);
end;

procedure TXmlNode.AttributeExchange(Index1, Index2: integer);
var
  Temp: UTF8String;
begin
  if (Index1 <> Index2) and
     (Index1 >= 0) and (Index1 < FAttributes.Count) and
     (Index2 >= 0) and (Index2 < FAttributes.Count) then
  begin
    Temp := FAttributes[Index1];
    FAttributes[Index1] := FAttributes[Index2];
    FAttributes[Index2] := Temp;
  end;
end;

function TXmlNode.AttributeIndexByname(const AName: UTF8String): integer;
// Return the index of the attribute with name AName, or -1 if not found
begin
  if assigned(FAttributes) then
    Result := FAttributes.IndexOfName(AName)
  else
    Result := -1;
end;

procedure TXmlNode.AttributesClear;
begin
  FreeAndNil(FAttributes);
end;

function TXmlNode.BufferLength: integer;
var
  BufData: UTF8String;
  BufPos: integer;
begin
  BufData := sdRemoveControlChars(FValue);
  case BinaryEncoding of
  xbeBinHex:
    begin
      Result := length(BufData) div 2;
      if Result * 2 <> length(BufData) then
        raise EFilerError.Create(sxeErrorCalcStreamLength);
    end;
  xbeBase64:
    begin
      Result := length(BufData) div 4;
      if Result * 4 <> length(BufData) then
        raise EFilerError.Create(sxeErrorCalcStreamLength);
      Result := Result * 3;
      // Check padding AnsiChars
      BufPos := length(BufData);
      if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      begin
        dec(BufPos);
        dec(Result);
        if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
          dec(Result);
      end;
    end;
  else
    Result := 0; // avoid compiler warning
  end;
end;

procedure TXmlNode.BufferRead(var Buffer; Count: Integer);
// Read data from XML binhex to the buffer
var
  BufData: UTF8String;
begin
  BufData := sdRemoveControlChars(FValue);
  case BinaryEncoding of
  xbeBinHex:
    DecodeBinHexBuf(BufData, Buffer, Count);
  xbeBase64:
    DecodeBase64Buf(BufData, Buffer, Count);
  end;
end;

procedure TXmlNode.BufferWrite(const Buffer; Count: Integer);
// Write data from the buffer to XML in binhex or base64 format
var
  BufData: UTF8String;
begin
  if Count > 0 then
    case BinaryEncoding of
    xbeBinHex:
      BufData := EncodeBinHexBuf(Buffer, Count);
    xbeBase64:
      BufData := EncodeBase64Buf(Buffer, Count);
    end;

  // For comformity with Base64, we must add linebreaks each 76 AnsiCharacters
  FValue := sdAddControlChars(BufData, GetLineFeed + GetIndent, 76);
end;

procedure TXmlNode.ChangeDocument(ADocument: TRelaxXml);
var
  i: integer;
begin
  FDocument := ADocument;
  for i := 0 to NodeCount - 1 do
    Nodes[i].ChangeDocument(ADocument);
end;

procedure TXmlNode.CheckCreateAttributesList;
begin
  if not assigned(FAttributes) then
    FAttributes := TsdUTF8StringList.Create;
end;

procedure TXmlNode.Clear;
begin
  // Name + value
  FName  := '';
  FValue := '';
  // Clear attributes and nodes
  AttributesClear;
  NodesClear;
end;

function TXmlNode.CompareNodeName(const NodeName: UTF8String): integer;
begin
  // Compare with FullPath or local name based on NodeName's first AnsiCharacter
  if length(NodeName) > 0 then
    if NodeName[1] = '/' then
    begin
      // FullPath
      Result := UTF8CompareText(FullPath, NodeName);
      exit;
    end;
  // local name
  Result := UTF8CompareText(Name, NodeName);
end;

constructor TXmlNode.Create(ADocument: TRelaxXml);
begin
  inherited Create;
  FDocument := ADocument;
end;

constructor TXmlNode.CreateName(ADocument: TRelaxXml; const AName: UTF8String);
begin
  Create(ADocument);
  Name := AName;
end;

constructor TXmlNode.CreateNameValue(ADocument: TRelaxXml; const AName, AValue: UTF8String);
begin
  Create(ADocument);
  Name := AName;
  ValueAsString := AValue;
end;

constructor TXmlNode.CreateType(ADocument: TRelaxXml;
  AType: TXmlElementType);
begin
  Create(ADocument);
  FElementType  := AType;
end;

procedure TXmlNode.Delete;
begin
  if assigned(Parent) then
    Parent.NodeRemove(Self);
end;

procedure TXmlNode.DeleteEmptyAttributes;
var
  i: integer;
  V: UTF8String;
begin
 for i := AttributeCount - 1 downto 0 do
 begin
   V := AttributeValue[i];
   if length(V) = 0 then
     FAttributes.Delete(i);
 end;
end;

procedure TXmlNode.DeleteEmptyNodes;
var
  i: integer;
  Node: TXmlNode;
begin
  for i := NodeCount - 1 downto 0 do
  begin
    Node := Nodes[i];
    // Recursive call
    Node.DeleteEmptyNodes;
    // Check if we should delete child node
    if Node.IsEmpty then
      NodeDelete(i);
  end;
end;

destructor TXmlNode.Destroy;
begin
  NodesClear;
  AttributesClear;
  inherited;
end;

function TXmlNode.FindNode(const NodeName: UTF8String): TXmlNode;
// Find the first node which has name NodeName. Contrary to the NodeByName
// function, this function will search the whole subnode tree, using the
// DepthFirst method.
var
  i: integer;
begin
  Result := nil;
  // Loop through all subnodes
  for i := 0 to NodeCount - 1 do
  begin
    Result := Nodes[i];
    // If the subnode has name NodeName then we have a result, exit
    if Result.CompareNodeName(NodeName) = 0 then
      exit;
    // If not, we will search the subtree of this node
    Result := Result.FindNode(NodeName);
    if assigned(Result) then
      exit;
  end;
end;

procedure TXmlNode.FindNodes(const NodeName: UTF8String; const AList: TList);
  // local
  procedure FindNodesRecursive(ANode: TXmlNode; AList: TList);
  var
    i: integer;
  begin
    with ANode do
      for i := 0 to NodeCount - 1 do
      begin
        if Nodes[i].CompareNodeName(NodeName) = 0 then
          AList.Add(Nodes[i]);
        FindNodesRecursive(Nodes[i], AList);
      end;
  end;
// main
begin
  AList.Clear;
  FindNodesRecursive(Self, AList);
end;

function TXmlNode.FloatAllowScientific: boolean;
begin
  if assigned(Document) then
    Result := Document.FloatAllowScientific
  else
    Result := cDefaultFloatAllowScientific;
end;

function TXmlNode.FloatSignificantDigits: integer;
begin
  if assigned(Document) then
    Result := Document.FloatSignificantDigits
  else
    Result := cDefaultFloatSignificantDigits;
end;

function TXmlNode.FromAnsiString(const s: AnsiString): UTF8String;
begin
  Result := sdAnsiToUtf8(s)
end;

function TXmlNode.FromUnicodeString(const W: UnicodeString): UTF8String;
begin
  Result := sdUnicodeToUtf8(W)
end;

function TXmlNode.GetAttributeByName(const AName: UTF8String): UTF8String;
begin
  if assigned(FAttributes) then
    Result := sdUTF8UnEscapeString(sdUTF8UnQuotedString(FAttributes.Values[AName]))
  else
    Result := '';
end;

function TXmlNode.GetAttributeByNameWide(const AName: UTF8String): UnicodeString;
begin
  Result := ToUnicodeString(GetAttributeByName(AName));
end;

function TXmlNode.GetAttributeCount: integer;
begin
  if assigned(FAttributes) then
    Result := FAttributes.Count
  else
    Result := 0;
end;

function TXmlNode.GetAttributeName(Index: integer): UTF8String;
begin
  if (Index >= 0) and (Index < AttributeCount) then
    Result := FAttributes.Names[Index];
end;

function TXmlNode.GetAttributePair(Index: integer): UTF8String;
begin
  if (Index >= 0) and (Index < AttributeCount) then
    Result := FAttributes[Index];
end;

function TXmlNode.GetAttributeValue(Index: integer): UTF8String;
var
  P: integer;
  S: UTF8String;
begin
  Result := '';
  if (Index >= 0) and (Index < AttributeCount) then
  begin
    S := FAttributes[Index];
    P := Utf8Pos('=', S);
    if P > 0 then
      Result := sdUTF8UnEscapeString(sdUTF8UnQuotedString(Copy(S, P + 1, MaxInt)));
  end;
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
  Result := StrToIntDef(string(GetAttributeValue(Index)), 0);
end;

function TXmlNode.GetAttributeValueAsUnicodeString(Index: integer): UnicodeString;
begin
  Result := ToUnicodeString(GetAttributeValue(Index));
end;

function TXmlNode.GetAttributeValueDirect(Index: integer): UTF8String;
var
  P: integer;
  S: UTF8String;
begin
  Result := '';
  if (Index >= 0) and (Index < AttributeCount) then
  begin
    S := FAttributes[Index];
    P := Utf8Pos('=', S);
    if P > 0 then
      Result := sdUTF8UnQuotedString(Copy(S, P + 1, MaxInt));
  end;
end;

function TXmlNode.GetBinaryEncoding: TBinaryEncodingType;
begin
  Result := xbeBinHex;
  if assigned(Document) then
    Result := Document.BinaryEncoding;
end;

function TXmlNode.GetBinaryString: RawByteString;
// Get the binary contents of this node as Base64 and return it as a RawByteString
var
  OldEncoding: TBinaryEncodingType;
begin
  // Set to base64
  OldEncoding := BinaryEncoding;
  try
    BinaryEncoding := xbeBase64;
    SetLength(Result, BufferLength);
    if length(Result) > 0 then
      BufferRead(Result[1], length(Result));
  finally
    BinaryEncoding := OldEncoding;
  end;
end;

function TXmlNode.GetCascadedName: UTF8String;
// Return the name+index and all predecessors with underscores to separate, in
// order to get a unique reference that can be used in filenames
var
  LName: UTF8String;
begin
  LName :=  UTF8String(Format('%s%.4d', [Name, StrToIntDef(string(AttributeByName['Index']), 0)]));
  if assigned(Parent) then
    Result := UTF8String(Format('%s_%s', [Parent.CascadedName, LName]))
  else
    Result := LName;
end;

function TXmlNode.GetFullPath: UTF8String;
// GetFullpath will return the complete path of the node from the root, e.g.
// /Root/SubNode1/SubNode2/ThisNode
begin
  Result := '/' + Name;
  if Treedepth > 0 then
    // Recursive call
    Result := Parent.GetFullPath + Result;
end;

function TXmlNode.GetIndent: UTF8String;
var
  i: integer;
begin
  if assigned(Document) then
  begin
    case Document.XmlFormat of
    xfCompact: Result := '';
    xfReadable:
      for i := 0 to TreeDepth - 1 do
        Result := Result + Document.IndentString;
    end; //case
  end else
    Result := ''
end;

function TXmlNode.GetLineFeed: UTF8String;
begin
  if assigned(Document) then
  begin
    case Document.XmlFormat of
    xfCompact:  Result := '';
    xfReadable: Result := #13#10;
    else
      Result := #10;
    end; //case
  end else
    Result := '';
end;

function TXmlNode.GetNodeCount: integer;
begin
  if Assigned(FNodes) then
    Result := FNodes.Count
  else
    Result := 0;
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < NodeCount) then
    Result := TXmlNode(FNodes[Index])
  else
    Result := nil;
end;

function TXmlNode.GetTotalNodeCount: integer;
var
  i: integer;
begin
  Result := NodeCount;
  for i := 0 to NodeCount - 1 do
    inc(Result, Nodes[i].TotalNodeCount);
end;

function TXmlNode.GetTreeDepth: integer;
begin
  Result := -1;
  if assigned(Parent) then
    Result := Parent.TreeDepth + 1;
end;

function TXmlNode.GetValueAsBool: boolean;
begin
  Result := sdStringToBool(FValue);
end;

function TXmlNode.GetValueAsDateTime: TDateTime;
begin
  Result := sdDateTimeFromString(ValueAsString, UseLocalBias);
end;

function TXmlNode.GetValueAsFloat: double;
var
  Code: integer;
begin
  val(string(sdUTF8StringReplace(FValue, ',', '.')), Result, Code);
  if Code > 0 then
    raise Exception.Create(sxeCannotConvertToFloat);
end;

function TXmlNode.GetValueAsInt64: int64;
begin
  Result := StrToInt64(string(FValue));
end;

function TXmlNode.GetValueAsInteger: integer;
begin
  Result := StrToInt(string(FValue));
end;

function TXmlNode.GetValueAsString: UTF8String;
begin
  if FElementType = xeNormal then
    Result := UnEscapeString(sdUTF8Trim(FValue))
  else
    Result := UnEscapeString(FValue);
end;

function TXmlNode.GetValueAsUnicodeString: UnicodeString;
begin
  Result := ToUnicodeString(ValueAsString);
end;

function TXmlNode.GetWriteOnDefault: boolean;
begin
  Result := True;
  if assigned(Document) then
    Result := Document.WriteOnDefault;
end;

function TXmlNode.HasAttribute(const AName: UTF8String): boolean;
begin
  if assigned(FAttributes) then
    Result := FAttributes.IndexOfName(AName) >= 0
  else
    Result := False;
end;

function TXmlNode.IndexInParent: integer;
// Retrieve our index in the parent's nodelist
begin
  Result := -1;
  if assigned(Parent) then
    Result := Parent.FNodes.IndexOf(Self);
end;

function TXmlNode.IsClear: boolean;
begin
  Result := (Length(FName) = 0) and IsEmpty;
end;

function TXmlNode.IsEmpty: boolean;
begin
  Result := (Length(FValue) = 0) and (NodeCount = 0) and (AttributeCount = 0);
end;

function TXmlNode.IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions;
  MismatchNodes: TList): boolean;
var
  i, Index: integer;
  NodeResult, ChildResult: boolean;
begin
  // Start with a negative result
  Result := False;
  NodeResult := False;
  if not assigned(ANode) then
    exit;

  // Assume childs equals other node's childs
  ChildResult := True;

  // child node names and values - this comes first to assure the lists are filled
  if (xcChildNames in Options) or (xcChildValues in Options) or (xcRecursive in Options) then
    for i := 0 to NodeCount - 1 do
    begin
      // Do child name check
      Index := ANode.NodeIndexByName(Nodes[i].Name);
      // Do we have the childnode in the other?
      if Index < 0 then
      begin
        // No we dont have it
        if xcChildNames in Options then
        begin
          if assigned(MismatchNodes) then MismatchNodes.Add(Nodes[i]);
          ChildResult := False;
        end;
      end else
      begin
        // Do child value check
        if xcChildValues in Options then
          if UTF8CompareText(Nodes[i].ValueAsString, ANode.Nodes[Index].ValueAsString) <> 0 then
          begin
            if assigned(MismatchNodes) then
              MismatchNodes.Add(Nodes[i]);
            ChildResult := False;
          end;
        // Do recursive check
        if xcRecursive in Options then
          if not Nodes[i].IsEqualTo(ANode.Nodes[Index], Options, MismatchNodes) then
            ChildResult := False;
      end;
    end;

  try
    // We assume there are differences
    NodeResult := False;

    // Node name, type and value
    if xcNodeName in Options then
      if UTF8CompareText(Name, ANode.Name) <> 0 then
        exit;

    if xcNodeType in Options then
      if ElementType <> ANode.ElementType then
        exit;

    if xcNodeValue in Options then
      if UTF8CompareText(ValueAsString, ANode.ValueAsString) <> 0 then
        exit;

    // attribute count
    if xcAttribCount in Options then
      if AttributeCount <> ANode.AttributeCount then
        exit;

    // attribute names and values
    if (xcAttribNames in Options) or (xcAttribValues in Options) then
      for i := 0 to AttributeCount - 1 do
      begin
        Index := ANode.AttributeIndexByName(AttributeName[i]);
        if Index < 0 then
          if xcAttribNames in Options then
            exit
          else
            continue;
        if xcAttribValues in Options then
          if UTF8CompareText(AttributeValue[i], ANode.AttributeValue[Index]) <> 0 then
            exit;
      end;

    // child node count
    if xcChildCount in Options then
      if NodeCount <> ANode.NodeCount then
        exit;

    // If we arrive here, it means no differences were found, return True
    NodeResult := True;

  finally

    Result := ChildResult and NodeResult;
    if (not NodeResult) and assigned(MismatchNodes) then
      MismatchNodes.Insert(0, Self);

  end;
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
  if assigned(ANode) then
  begin
    ANode.Parent := Self;
    ANode.ChangeDocument(Document);
    if not assigned(FNodes) then
      FNodes := TList.Create;
    Result := FNodes.Add(ANode);
  end else
    Result := -1;
end;

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName, AttribValue: UTF8String;
  ShouldRecurse: boolean): TXmlNode;
// This function returns a pointer to the first subnode that has an attribute with
// name AttribName and value AttribValue.
var
  i: integer;
  Node: TXmlNode;
begin
  Result := nil;
  // Find all nodes that are potential results
  for i := 0 to NodeCount - 1 do
  begin
    Node := Nodes[i];
    if (UTF8CompareText(Node.Name, NodeName) = 0) and
        Node.HasAttribute(AttribName) and
       (UTF8CompareText(Node.AttributeByName[AttribName], AttribValue) = 0) then
    begin
      Result := Node;
      exit;
    end;
    // Recursive call
    if ShouldRecurse then
      Result := Node.NodeByAttributeValue(NodeName, AttribName, AttribValue, True);
    if assigned(Result) then
      exit;
  end;
end;

function TXmlNode.NodeByElementType(ElementType: TXmlElementType): TXmlNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to NodeCount - 1 do
    if Nodes[i].ElementType = ElementType then
    begin
      Result := Nodes[i];
      exit;
    end;
end;

function TXmlNode.NodeByName(const AName: UTF8String): TXmlNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to NodeCount - 1 do
    if UTF8CompareText(Nodes[i].Name, AName) = 0 then
    begin
      Result := Nodes[i];
      exit;
    end;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
  if (Index >= 0) and (Index < NodeCount) then
  begin
    TXmlNode(FNodes[Index]).Free;
    FNodes.Delete(Index);
  end;
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
  if (Index1 >= 0) and (Index1 < Nodecount) and
     (Index2 >= 0) and (Index2 < Nodecount) then
    FNodes.Exchange(Index1, Index2);
end;

function TXmlNode.NodeExtract(ANode: TXmlNode): TXmlNode;
var
  Index: integer;
begin
  // Compatibility with Delphi4
  Result := nil;
  if assigned(FNodes) then
  begin
    Index := FNodes.IndexOf(ANode);
    if Index >= 0 then begin
      Result := ANode;
      FNodes.Delete(Index);
    end;
  end;
end;

function TXmlNode.NodeFindOrCreate(const AName: UTF8String): TXmlNode;
// Find the node with AName, and if not found, add new one
begin
  Result := NodeByName(AName);
  if not assigned(Result) then
    Result := NodeNew(AName);
end;

function TXmlNode.NodeIndexByName(const AName: UTF8String): integer;
begin
  Result := 0;
  while Result < NodeCount do
  begin
    if UTF8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = NodeCount then
    Result := -1;
end;

function TXmlNode.NodeIndexByNameFrom(const AName: UTF8String; AFrom: integer): integer;
begin
  Result := AFrom;
  while Result < NodeCount do
  begin
    if UTF8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = NodeCount then
    Result := -1;
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  if assigned(ANode) and assigned(FNodes) then
    Result := FNodes.IndexOf(ANode)
  else
    Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
// Insert the node ANode at location Index in the list.
begin
  if not assigned(ANode) then
    exit;
  if (Index >=0) and (Index <= NodeCount) then
  begin
    if not assigned(FNodes) then
      FNodes := TList.Create;
    ANode.Parent := Self;
    FNodes.Insert(Index, ANode);
  end;
end;

function TXmlNode.NodeNew(const AName: UTF8String): TXmlNode;
// Add a new child node and return its pointer
begin
  Result := Nodes[NodeAdd(TXmlNode.CreateName(Document, AName))];
end;

function TXmlNode.NodeNewAtIndex(Index: integer; const AName: UTF8String): TXmlNode;
// Create a new node with AName, and insert it into the subnode list at location
// Index, and return a pointer to it.
begin
  if (Index >= 0) and (Index <= NodeCount) then
  begin
    Result := TXmlNode.CreateName(Document, AName);
    NodeInsert(Index, Result);
  end else
    Result := nil;
end;

function TXmlNode.NodeRemove(ANode: TxmlNode): integer;
begin
  Result := NodeIndexOf(ANode);
  if Result >= 0 then
    NodeDelete(Result);
end;

procedure TXmlNode.NodesByName(const AName: UTF8String; const AList: TList);
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

procedure TXmlNode.NodesClear;
var
  i: integer;
begin
  for i := 0 to NodeCount - 1 do
    TXmlNode(FNodes[i]).Free;
  FreeAndNil(FNodes);
end;

procedure TXmlNode.ParseTag(const AValue: UTF8String; TagStart, TagClose: integer);
var
  LItems: TsdUTF8StringList;
begin
  // Create a list to hold string items
  LItems := TsdUTF8StringList.Create;
  try
    sdUTF8ParseAttributes(AValue, TagStart, TagClose, LItems);

    // Determine name, attributes or value for each element type
    case ElementType of
    xeDeclaration:
      FName := 'xml';
    xeStyleSheet:
      begin
        FName := 'xml-stylesheet';
        // We also set this as the value for use in "StyleSheetString"
        ValueDirect := sdUTF8Trim(copy(AValue, TagStart, TagClose - TagStart));
      end;
    else
      // First item is the name - is it there?
      if LItems.Count = 0 then
        raise EFilerError.Create(sxeMissingElementName);

      // Set the name - using the element instead of property for speed
      FName := LItems[0];
      LItems.Delete(0);
    end;//case

    // Any attributes?
    if LItems.Count > 0 then
    begin
      CheckCreateAttributesList;
      FAttributes.Assign(LItems);
    end;

  finally
    LItems.Free;
  end;
end;

function TXmlNode.QualifyAsDirectNode: boolean;
// If this node qualifies as a direct node when writing, we return True.
// A direct node may have attributes, but no value or subnodes. Furhtermore,
// the root node will never be displayed as a direct node.
begin
  Result :=
    (Length(FValue) = 0) and
    (NodeCount = 0) and
    (ElementType = xeNormal) and
    not UseFullNodes and
    (TreeDepth > 0);
end;

function TXmlNode.ReadAttributeBool(const AName: UTF8String; ADefault: boolean): boolean;
var
  V: UTF8String;
begin
  Result := ADefault;
  V := AttributeByName[AName];
  if Length(V) = 0 then
    exit;

  try
    Result := sdStringToBool(V);
  except
    Result := ADefault;
  end;
end;

function TXmlNode.ReadAttributeDateTime(const AName: UTF8String; ADefault: TDateTime): TDateTime;
var
  V: UTF8String;
begin
  Result := ADefault;
  V := AttributeByName[AName];
  if Length(V) = 0 then
    exit;

  try
    Result := sdDateTimeFromStringDefault(V, ADefault, UseLocalBias);
  except
    Result := ADefault;
  end;
end;

function TXmlNode.ReadAttributeFloat(const AName: UTF8String; ADefault: double): double;
var
  V: UTF8String;
  Code: integer;
begin
  V := AttributeByName[AName];
  val(string(sdUTF8StringReplace(V, ',', '.')), Result, Code);
  if Code > 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeInteger(const AName: UTF8String; ADefault: integer): integer;
begin
  Result := StrToIntDef(string(AttributeByName[AName]), ADefault);
end;

function TXmlNode.ReadAttributeInt64(const AName: UTF8String; ADefault: int64): int64;
begin
  Result := StrToInt64Def(string(AttributeByName[AName]), ADefault);
end;

function TXmlNode.ReadAttributeString(const AName: UTF8String; const ADefault: UTF8String): UTF8String;
begin
  Result := AttributeByName[AName];
  if length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadBool(const AName: UTF8String; ADefault: boolean): boolean;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := Nodes[Index].ValueAsBoolDef(ADefault);
end;

procedure TXmlNode.ReadBrush(const AName: UTF8String; ABrush: TBrush);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then with Child do
  begin
    // Read values
    ABrush.Color  := ReadColor('Color', clWhite);
    ABrush.Style  := TBrushStyle(ReadInteger('Style', integer(bsSolid)));
  end else
  begin
    // Defaults
    ABrush.Bitmap := nil;
    ABrush.Color  := clWhite;
    ABrush.Style  := bsSolid;
  end;
end;

function TXmlNode.ReadColor(const AName: UTF8String; ADefault: TColor): TColor;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := StrToInt(string(Nodes[Index].ValueAsString));
end;

function TXmlNode.ReadDateTime(const AName: UTF8String; ADefault: TDateTime): TDateTime;
// Date MUST always be written in this format:
// YYYY-MM-DD (if just date) or
// YYYY-MM-DDThh:mm:ss.sssZ (if date and time. The Z stands for universal time
// zone. Since Delphi's TDateTime does not give us a clue about the timezone,
// this is the easiest solution)
// This format SHOULD NOT be changed, to avoid all kinds of
// conversion errors in future.
// This format is compatible with the W3C date/time specification as found here:
// http://www.w3.org/TR/NOTE-datetime
begin
  Result := sdDateTimeFromStringDefault(ReadString(AName, ''), ADefault, UseLocalBias);
end;

function TXmlNode.ReadFloat(const AName: UTF8String; ADefault: double): double;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := Nodes[Index].ValueAsFloatDef(ADefault);
end;

procedure TXmlNode.ReadFont(const AName: UTF8String; AFont: TFont);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  AFont.Style := [];
  if assigned(Child) then with Child do
  begin
    // Read values
    AFont.Name  := string(ReadString('Name', 'Arial'));
    AFont.Color := ReadColor('Color', clBlack);
    AFont.Size  := ReadInteger('Size', 14);
    if ReadBool('Bold', False)      then AFont.Style := AFont.Style + [fsBold];
    if ReadBool('Italic', False)    then AFont.Style := AFont.Style + [fsItalic];
    if ReadBool('Underline', False) then AFont.Style := AFont.Style + [fsUnderline];
    if ReadBool('Strikeout', False) then AFont.Style := AFont.Style + [fsStrikeout];
  end else
  begin
    // Defaults
    AFont.Name  := 'Arial';
    AFont.Color := clBlack;
    AFont.Size  := 14;
  end;
end;

procedure TXmlNode.ReadFromStream(S: TStream);
// Read the node from the starting "<" until the closing ">" from the stream in S.
// This procedure also calls OnNodeNew and OnNodeLoaded events
var
  Ch: AnsiChar;
  i: integer;
  TagIndex: integer;
  V: UTF8String;
  Len: integer;
  Node: TXmlNode;
  NodeValue: UTF8String;
  ValuePos, ValueLen: integer;
  ClosePos: integer;
  HasCR: boolean;
  HasSubtags: boolean;
  Words: TsdUTF8StringList;
  IsDirect: boolean;
  Reader: TsdSurplusReader;
  // local
  procedure AddCharDataNode(PreserveWhiteSpace: boolean);
  var
    V: UTF8String;
    Node: TXmlNode;
    L: integer;
  begin
    // Add all text up till now as xeCharData
    if ValuePos > 0 then
    begin
      V := copy(NodeValue, 1, ValuePos);

      if PreserveWhiteSpace then
        L := length(V)
      else
        L := length(sdUTF8Trim(V));

      if L > 0 then
      begin
        Node := TXmlNode.CreateType(Document, xeCharData);
        Node.ValueDirect := V;
        NodeAdd(Node);
      end;
      ValuePos := 0;
    end;
  end;
// Main
begin
  // Check if we aborted parsing
  if AbortParsing then
    exit;
  // Clear this node first
  Clear;
  // Initial reserve textual value: just 80 AnsiCharacters which is OK for most short values
  ValuePos := 0;
  ValueLen := 80;
  SetLength(NodeValue, ValueLen);
  HasCR := False;
  HasSubTags := False;
  Reader := TsdSurplusReader.Create(S);
  try
    // Trailing blanks/controls AnsiChars?
    if not Reader.ReadCharSkipBlanks(Ch) then
      exit;

    // What is it?
    if Ch = '<' then
    begin
      // A tag - which one?
      TagIndex := ReadOpenTag(Reader);
      if TagIndex >= 0 then
      begin
        try
          ElementType := cTags[TagIndex].Style;
          case ElementType of
          xeNormal, xeDeclaration, xeStyleSheet:
            begin
              // These tags we will process
              ReadStringFromStreamUntil(Reader, cTags[TagIndex].Close, V, True);
              Len := length(V);

              // Is it a direct tag?
              IsDirect := False;
              if (ElementType = xeNormal) and (Len > 0) then
                if V[Len] = '/' then
                begin
                  dec(Len);
                  IsDirect := True;
                end;
              ParseTag(V, 1, Len + 1);

              // Here we know our name so good place to call OnNodeNew event
              if assigned(Document) then
              begin
                Document.DoNodeNew(Self);
                if AbortParsing then
                  exit;
              end;

              // Now the tag can be a direct close - in that case we're finished
              if IsDirect or (ElementType in [xeDeclaration, xeStyleSheet]) then
                exit;

              // Process rest of tag
              repeat

                // Read AnsiCharacter from stream
                if S.Read(Ch, 1) <> 1 then
                  raise EFilerError.CreateFmt(sxeMissingCloseTag, [Name]);

                // Is there a subtag?
                if Ch = '<' then
                begin
                  if not Reader.ReadCharSkipBlanks(Ch) then
                    raise EFilerError.CreateFmt(sxeMissingDataAfterGreaterThan, [Name]);
                  if Ch = '/' then
                  begin

                    // This seems our closing tag
                    if not ReadStringFromStreamUntil(Reader, '>', V, True) then
                      raise EFilerError.CreateFmt(sxeMissingLessThanInCloseTag, [Name]);
                    if UTF8CompareText(sdUTF8Trim(V), Name) <> 0 then
                      raise EFilerError.CreateFmt(sxeIncorrectCloseTag, [Name]);
                    V := '';
                    break;

                  end else
                  begin

                    // Add all text up till now as xeCharData
                    AddCharDataNode(False);

                    // Reset the HasCR flag if we add node, we only want to detect
                    // the CR after last subnode
                    HasCR := False;

                    // This is a subtag... so create it and let it process
                    HasSubTags := True;
                    S.Seek(-2, soCurrent);
                    Node := TXmlNode.Create(Document);
                    NodeAdd(Node);
                    Node.ReadFromStream(S);

                    // Check for dropping comments
                    if assigned(Document) and Document.DropCommentsOnParse and
                       (Node.ElementType = xeComment) then
                      NodeDelete(NodeIndexOf(Node));

                  end;
                end else
                begin

                  // If we detect a CR we will set the flag. This will signal the fact
                  // that this XML file was saved with xfReadable
                  if Ch = #13 then
                    HasCR := True;

                  // Add the AnsiCharacter to the node value buffer.
                  inc(ValuePos);
                  if ValuePos > ValueLen then
                  begin
                    inc(ValueLen, cNodeValueBuf);
                    SetLength(NodeValue, ValueLen);
                  end;
                  NodeValue[ValuePos] := Ch;

                end;
              until False or AbortParsing;

              // Add all text up till now as xeText
              AddCharDataNode(not HasSubtags);

              // Check AnsiCharData nodes, remove trailing CRLF + indentation if we
              // were in xfReadable mode
              if HasSubtags and HasCR then
              begin
                for i := 0 to NodeCount - 1 do
                  if Nodes[i].ElementType = xeCharData then
                  begin
                    ClosePos := length(Nodes[i].FValue);
                    while (ClosePos > 0) and (Nodes[i].FValue[ClosePos] in [#10, #13, ' ']) do
                      dec(ClosePos);
                    Nodes[i].FValue := copy(Nodes[i].FValue, 1, ClosePos);
                  end;
              end;

              // If the first node is xeCharData we use it as ValueDirect
              if NodeCount > 0 then
                if Nodes[0].ElementType = xeCharData then
                begin
                  ValueDirect := Nodes[0].ValueDirect;
                  NodeDelete(0);
                end;

            end;
          xeDocType:
            begin
              Name := 'DTD';
              if assigned(Document) then
              begin
                Document.DoNodeNew(Self);
                if AbortParsing then
                  exit;
              end;
              // Parse DTD
              if assigned(Document) then
                Document.ParseDTD(Self, S);
            end;
          xeElement, xeAttList, xeEntity, xeNotation:
            begin
              // DTD elements
              ReadStringFromStreamWithQuotes(S, cTags[TagIndex].Close, V);
              Len := length(V);
              Words := TsdUTF8StringList.Create;
              try
                sdUTF8ParseAttributes(V, 1, Len + 1, Words);
                if Words.Count > 0 then
                begin
                  Name := Words[0];
                  Words.Delete(0);
                end;
                ValueDirect := sdUTF8Trim(Words.Text);
              finally
                Words.Free;
              end;
              if assigned(Document) then
              begin
                Document.DoNodeNew(Self);
                if AbortParsing then
                  exit;
              end;
            end;
          else
            case ElementType of
            xeComment:  Name := 'Comment';
            xeCData:    Name := 'CData';
            xeExclam:   Name := 'Special';
            xeQuestion: Name := 'Special';
            else
              Name := 'Unknown';
            end; //case

            // Here we know our name so good place to call OnNodeNew
            if assigned(Document) then
            begin
              Document.DoNodeNew(Self);
              if AbortParsing then
                exit;
            end;

            // In these cases just get all data up till the closing tag
            ReadStringFromStreamUntil(Reader, cTags[TagIndex].Close, V, False);
            ValueDirect := V;
          end;//case
        finally
          // Call the OnNodeLoaded and OnProgress events
          if assigned(Document) and not AbortParsing then
          begin
            Document.DoProgress(S.Position);
            Document.DoNodeLoaded(Self);
          end;
        end;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TXmlNode.ReadFromString(const AValue: UTF8String);
var
  S: TStream;
begin
  S := TsdUTF8StringStream.Create(AValue);
  try
    ReadFromStream(S);
  finally
    S.Free;
  end;
end;

function TXmlNode.ReadInt64(const AName: UTF8String; ADefault: int64): int64;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := Nodes[Index].ValueAsInt64Def(ADefault);
end;

function TXmlNode.ReadInteger(const AName: UTF8String; ADefault: integer): integer;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := Nodes[Index].ValueAsIntegerDef(ADefault);
end;

procedure TXmlNode.ReadPen(const AName: UTF8String; APen: TPen);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then with Child do
  begin
    // Read values
    APen.Color := ReadColor('Color', clBlack);
    APen.Mode  := TPenMode(ReadInteger('Mode', integer(pmCopy)));
    APen.Style := TPenStyle(ReadInteger('Style', integer(psSolid)));
    APen.Width := ReadInteger('Width', 1);
  end else
  begin
    // Defaults
    APen.Color := clBlack;
    APen.Mode := pmCopy;
    APen.Style := psSolid;
    APen.Width := 1;
  end;
end;

function TXmlNode.ReadString(const AName: UTF8String; const ADefault: UTF8String): UTF8String;
var
  Index: integer;
begin
  Result := ADefault;
  Index := NodeIndexByName(AName);
  if Index >= 0 then
    Result := Nodes[Index].ValueAsString;
end;

function TXmlNode.ReadUnicodeString(const AName: UTF8String; const ADefault: UnicodeString): UnicodeString;
begin
  Result := ToUnicodeString(ReadString(AName, FromUnicodeString(ADefault)));
end;

procedure TXmlNode.ResolveEntityReferences;
// Replace any entity references by the entities, and parse the new content if any
  // local
  function SplitReference(const AValue: UTF8String; var Text1, Text2: UTF8String): UTF8String;
  var
    P: integer;
  begin
    Result := '';
    P := UTF8Pos('&', AValue);
    Text1 := '';
    Text2 := AValue;
    if P = 0 then
      exit;
    Text1 := copy(AValue, 1, P - 1);
    Text2 := copy(AValue, P + 1, length(AValue));
    P := UTF8Pos(';', Text2);
    if P = 0 then
      exit;
    Result := copy(Text2, 1, P - 1);
    Text2 := copy(Text2, P + 1, length(Text2));
  end;
  // local
  function ReplaceEntityReferenceByNodes(ARoot: TXmlNode; const AValue: UTF8String; var InsertPos: integer; var Text1, Text2: UTF8String): boolean;
  var
    Reference: UTF8String;
    Entity: UTF8String;
    Node: TXmlNode;
    S: TStream;
  begin
    Result := False;
    Reference := SplitReference(AValue, Text1, Text2);
    if (length(Reference) = 0) or not assigned(Document) then
      exit;

    // Lookup entity references
    Entity := Document.EntityByName[Reference];

    // Does the entity contain markup?
    if (length(Entity) > 0) and (UTF8Pos('<', Entity) > 0) then
    begin
      S := TsdUTF8StringStream.Create(Entity);
      try
        while S.Position < S.Size do
        begin
          Node := TXmlNode.Create(Document);
          Node.ReadFromStream(S);
          if Node.IsEmpty then
            Node.Free
          else
          begin
            ARoot.NodeInsert(InsertPos, Node);
            inc(InsertPos);
            Result := True;
          end;
        end;
      finally
        S.Free;
      end;
    end;
  end;
// main
var
  i: integer;
  InsertPos: integer;
  Text1, Text2: UTF8String;
  Node: TXmlNode;
  V, Reference, Replace, Entity, First, Last: UTF8String;
begin
  if length(FValue) > 0 then
  begin
    // Different behaviour for xeNormal and xeCharData
    if ElementType = xeNormal then
    begin
      InsertPos := 0;
      if ReplaceEntityReferenceByNodes(Self, FValue, InsertPos, Text1, Text2) then
      begin
        FValue := Text1;
        if length(sdUTF8Trim(Text2)) > 0 then
        begin
          Node := TXmlNode.CreateType(Document, xeCharData);
          Node.ValueDirect := Text2;
          NodeInsert(InsertPos, Node);
        end;
      end;
    end else if (ElementType = xeCharData) and assigned(Parent) then
    begin
      InsertPos := Parent.NodeIndexOf(Self);
      if ReplaceEntityReferenceByNodes(Parent, FValue, InsertPos, Text1, Text2) then
      begin
        FValue := Text1;
        if length(sdUTF8Trim(FValue)) = 0 then
          FValue := '';
        if length(sdUTF8Trim(Text2)) > 0 then
        begin
          Node := TXmlNode.CreateType(Document, xeCharData);
          Node.ValueDirect := Text2;
          Parent.NodeInsert(InsertPos, Node);
        end;
      end;
    end;
  end;

  // Do attributes
  for i := 0 to AttributeCount - 1 do
  begin
    Last := AttributeValue[i];
    V := '';
    repeat
      Reference := SplitReference(Last, First, Last);
      Replace := '';
      if length(Reference) > 0 then
      begin
        Entity := Document.EntityByName[Reference];
        if length(Entity) > 0 then
           Replace := Entity
        else
          Replace := '&' + Reference + ';';
      end;
      V := V + First + Replace;
    until length(Reference) = 0;
    V := V + Last;
    AttributeValue[i] := V;
  end;

  // Do childnodes too
  i := 0;
  while i < NodeCount do
  begin
    Nodes[i].ResolveEntityReferences;
    inc(i);
  end;

  // Check for empty AnsiCharData nodes
  for i := NodeCount - 1 downto 0 do
    if (Nodes[i].ElementType = xeCharData) and (length(Nodes[i].ValueDirect) = 0) then
      NodeDelete(i);
end;

procedure TXmlNode.SetAttributeByName(const AName, Value: UTF8String);
begin
  CheckCreateAttributesList;
  FAttributes.Values[AName] := sdUTF8QuotedString(sdUTF8EscapeString(Value));
end;

procedure TXmlNode.SetAttributeByNameWide(const AName: UTF8String; const Value: UnicodeString);
begin
  SetAttributeByName(AName, FromUnicodeString(Value));
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: UTF8String);
var
  S: UTF8String;
  P: integer;
begin
  if (Index >= 0) and (Index < AttributeCount) then
  begin
    S := FAttributes[Index];
    P := Utf8Pos('=', S);
    if P > 0 then
      FAttributes[Index] := Value + '=' + Copy(S, P + 1, MaxInt);
  end;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: UTF8String);
begin
  if (Index >= 0) and (Index < AttributeCount) then
    FAttributes[Index] := AttributeName[Index] + '=' +
      sdUTF8QuotedString(sdUTF8EscapeString(Value));
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
  SetAttributeValue(Index, IntToUTF8Str(Value));
end;

procedure TXmlNode.SetAttributeValueAsUnicodeString(Index: integer;
  const Value: UnicodeString);
begin
  SetAttributeValue(Index, FromUnicodeString(Value));
end;

procedure TXmlNode.SetAttributeValueDirect(Index: integer; const Value: UTF8String);
begin
  if (Index >= 0) and (Index < AttributeCount) then
    FAttributes[Index] := AttributeName[Index] + '=' +
      sdUTF8QuotedString(Value);
end;

procedure TXmlNode.SetBinaryEncoding(const Value: TBinaryEncodingType);
begin
  if assigned(Document) then
    Document.BinaryEncoding := Value;
end;

procedure TXmlNode.SetBinaryString(const Value: RawByteString);
var
  OldEncoding: TBinaryEncodingType;
begin
  // Set to base64
  OldEncoding := BinaryEncoding;
  try
    BinaryEncoding := xbeBase64;
    if length(Value) = 0 then
    begin
      ValueAsString := '';
      exit;
    end;
    // fill the buffer
    BufferWrite(Value[1], length(Value));
  finally
    BinaryEncoding := OldEncoding;
  end;
end;

procedure TXmlNode.SetName(const Value: UTF8String);
var
  i: integer;
begin
  if FName <> Value then
  begin
    // Check if the name abides the rules. We will be very forgiving here and
    // just accept any name that at least does not contain control AnsiCharacters
    for i := 1 to length(Value) do
      if Value[i] in cControlChars then
        raise Exception.Create(Format(sxeIllegalCharInNodeName, [Value]));
    FName := Value;
  end;
end;

procedure TXmlNode.SetValueAsBool(const Value: boolean);
begin
  FValue := sdStringFromBool(Value);
end;

procedure TXmlNode.SetValueAsDateTime(const Value: TDateTime);
begin
  ValueAsString := sdDateTimeToString(Value, UseLocalBias);
end;

procedure TXmlNode.SetValueAsFloat(const Value: double);
begin
  FValue := sdWriteNumber(Value, FloatSignificantDigits, FloatAllowScientific);
end;

procedure TXmlNode.SetValueAsInt64(const Value: int64);
begin
  FValue := Int64ToUTF8Str(Value);
end;

procedure TXmlNode.SetValueAsInteger(const Value: integer);
begin
  FValue := IntToUTF8Str(Value);
end;

procedure TXmlNode.SetValueAsString(const AValue: UTF8String);
begin
  FValue := sdUTF8EscapeString(AValue);
end;

procedure TXmlNode.SetValueAsUnicodeString(const Value: UnicodeString);
begin
  ValueAsString := FromUnicodeString(Value);
end;

procedure TXmlNode.SortChildNodes(Compare: TXMLNodeCompareFunction; Info: TPointer);
// Sort the child nodes using the quicksort algorithm
  //local
  function DoNodeCompare(Node1, Node2: TXmlNode): integer;
  begin
    if assigned(Compare) then
      Result := Compare(Node1, Node2, Info)
    else
      if assigned(Document) and assigned(Document.OnNodeCompare) then
        Result := Document.OnNodeCompare(Document, Node1, Node2, Info)
      else
        Result := UTF8CompareText(Node1.Name, Node2.Name);
  end;
  // local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid:= (Lo + Hi) div 2;
    repeat
      while DoNodeCompare(Nodes[Lo], Nodes[Mid]) < 0 do
        Inc(Lo);
      while DoNodeCompare(Nodes[Hi], Nodes[Mid]) > 0 do
        Dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        NodeExchange(Lo, Hi);
        if Mid = Lo then
          Mid := Hi
        else
          if Mid = Hi then
            Mid := Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then
      QuickSort(iLo, Hi);
    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;
// main
begin
  if NodeCount > 1 then
    QuickSort(0, NodeCount - 1);
end;

function TXmlNode.ToUnicodeString(const s: UTF8String): UnicodeString;
begin
  Result := sdUtf8ToUnicode(s)
end;

function TXmlNode.UnescapeString(const AValue: UTF8String): UTF8String;
begin
  Result := sdUTF8UnEscapeString(AValue)
end;

function TXmlNode.UseFullNodes: boolean;
begin
  Result := False;
  if assigned(Document) then
    Result := Document.UseFullNodes;
end;

function TXmlNode.UseLocalBias: Boolean;
begin
  Result := False;
  if Assigned(Document) then
    Result := Document.UseLocalBias;
end;

function TXmlNode.ValueAsBoolDef(ADefault: boolean): boolean;
var
  Ch: AnsiChar;
begin
  Result := ADefault;
  if Length(FValue) = 0 then
    exit;
  Ch := sdUpCase(FValue[1]);
  if Ch in ['T', 'Y'] then
  begin
    Result := True;
    exit;
  end;
  if Ch in ['F', 'N'] then
  begin
    Result := False;
    exit;
  end;
end;

function TXmlNode.ValueAsDateTimeDef(ADefault: TDateTime): TDateTime;
begin
  Result := sdDateTimeFromStringDefault(ValueAsString, ADefault, UseLocalBias);
end;

function TXmlNode.ValueAsFloatDef(ADefault: double): double;
var
  Code: integer;
begin
  try
    val(string(sdUTF8StringReplace(FValue, ',', '.')), Result, Code);
    if Code > 0 then
      Result := ADefault;
  except
    Result := ADefault;
  end;
end;

function TXmlNode.ValueAsInt64Def(ADefault: int64): int64;
begin
  Result := StrToInt64Def(string(FValue), ADefault);
end;

function TXmlNode.ValueAsIntegerDef(ADefault: integer): integer;
begin
  Result := StrToIntDef(string(FValue), ADefault);
end;

procedure TXmlNode.WriteAttributeBool(const AName: UTF8String; AValue: boolean; ADefault: boolean);
var
  Index: integer;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    if Index >= 0 then
      AttributeValue[Index] := sdStringFromBool(AValue)
    else
      AttributeAdd(AName, sdStringFromBool(AValue));
  end;
end;

procedure TXmlNode.WriteAttributeDateTime(const AName: UTF8String; AValue, ADefault: TDateTime);
var
  Index: integer;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    if Index >= 0 then
      AttributeValue[Index] := sdDateTimeToString(AValue, UseLocalBias)
    else
      AttributeAdd(AName, sdDateTimeToString(AValue, UseLocalBias));
  end;
end;

procedure TXmlNode.WriteAttributeFloat(const AName: UTF8String; AValue, ADefault: double);
var
  Index: integer;
  S: UTF8String;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    S := sdWriteNumber(AValue, FloatSignificantDigits, FloatAllowScientific);
    if Index >= 0 then
      AttributeValue[Index] := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeInteger(const AName: UTF8String; AValue: integer; ADefault: integer);
var
  Index: integer;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    if Index >= 0 then
      AttributeValue[Index] := IntToUTF8Str(AValue)
    else
      AttributeAdd(AName, IntToUTF8Str(AValue));
  end;
end;

procedure TXmlNode.WriteAttributeInt64(const AName: UTF8String; const AValue: int64; ADefault: int64);
var
  Index: integer;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    if Index >= 0 then
      AttributeValue[Index] := Int64ToUTF8Str(AValue)
    else
      AttributeAdd(AName, Int64ToUTF8Str(AValue));
  end;
end;

procedure TXmlNode.WriteAttributeString(const AName, AValue, ADefault: UTF8String);
var
  Index: integer;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    Index := AttributeIndexByName(AName);
    if Index >= 0 then
      AttributeValue[Index] := AValue
    else
      AttributeAdd(AName, AValue);
  end;
end;

procedure TXmlNode.WriteBool(const AName: UTF8String; AValue: boolean; ADefault: boolean);
const
  cBoolValues: array[boolean] of UTF8String = ('False', 'True');
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := cBoolValues[AValue];
end;

procedure TXmlNode.WriteBrush(const AName: UTF8String; ABrush: TBrush);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', ABrush.Color, clBlack);
    WriteInteger('Style', integer(ABrush.Style), 0);
  end;
end;

procedure TXmlNode.WriteColor(const AName: UTF8String; AValue, ADefault: TColor);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteHex(AName, ColorToRGB(AValue), 8, 0);
end;

procedure TXmlNode.WriteDateTime(const AName: UTF8String; AValue, ADefault: TDateTime);
// Date MUST always be written in this format:
// YYYY-MM-DD (if just date) or
// YYYY-MM-DDThh:mm:ss.sssZ (if date and time. The Z stands for universal time
// zone. Since Delphi's TDateTime does not give us a clue about the timezone,
// this is the easiest solution)
// This format SHOULD NOT be changed, to avoid all kinds of
// conversion errors in future.
// This format is compatible with the W3C date/time specification as found here:
// http://www.w3.org/TR/NOTE-datetime
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteString(AName, sdDateTimeToString(AValue, UseLocalBias), '');
end;

procedure TXmlNode.WriteFloat(const AName: UTF8String; AValue: double; ADefault: double);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := sdWriteNumber(AValue, FloatSignificantDigits, FloatAllowScientific);
end;

procedure TXmlNode.WriteFont(const AName: UTF8String; AFont: TFont);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteString('Name', UTF8String(AFont.Name), 'Arial');
    WriteColor('Color', AFont.Color, clBlack);
    WriteInteger('Size', AFont.Size, 14);
    WriteBool('Bold', fsBold in AFont.Style, False);
    WriteBool('Italic', fsItalic in AFont.Style, False);
    WriteBool('Underline', fsUnderline in AFont.Style, False);
    WriteBool('Strikeout', fsStrikeout in AFont.Style, False);
  end;
end;

procedure TXmlNode.WriteHex(const AName: UTF8String; AValue, Digits: integer; ADefault: integer);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := '$' + UTF8String(IntToHex(AValue, Digits));
end;

function TXmlNode.WriteInnerTag: UTF8String;
// Write the inner part of the tag, the one that contains the attributes
var
  i: integer;
begin
  Result := '';
  // Attributes
  for i := 0 to AttributeCount - 1 do
    // Here we used to prevent empty attributes, but in fact, empty attributes
    // should be allowed because sometimes they're required
    Result := Result + ' ' + AttributePair[i];
  // End of tag - direct nodes get an extra "/"
  if QualifyAsDirectNode then
    Result := Result + '/';
end;

procedure TXmlNode.WriteInt64(const AName: UTF8String; AValue, ADefault: int64);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := Int64ToUTF8Str(AValue);
end;

procedure TXmlNode.WriteInteger(const AName: UTF8String; AValue: integer; ADefault: integer);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := IntToUTF8Str(AValue);
end;

procedure TXmlNode.WritePen(const AName: UTF8String; APen: TPen);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', APen.Color, clBlack);
    WriteInteger('Mode', integer(APen.Mode), 0);
    WriteInteger('Style', integer(APen.Style), 0);
    WriteInteger('Width', APen.Width, 0);
  end;
end;

procedure TXmlNode.WriteString(const AName, AValue: UTF8String; const ADefault: UTF8String);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    with NodeFindOrCreate(AName) do
      ValueAsString := AValue;
end;

procedure TXmlNode.WriteToStream(S: TStream);
var
  i: integer;
  Indent: UTF8String;
  LFeed: UTF8String;
  Line: UTF8String;
  ThisNode, NextNode: TXmlNode;
  AddLineFeed: boolean;
begin
  Indent := GetIndent;
  LFeed := GetLineFeed;

  // Write indent
  Line := Indent;

  // Write the node - distinguish node type
  case ElementType of
  xeDeclaration: // XML declaration <?xml{declaration}?>
    begin
      // Explicitly delete empty attributes in the declaration,
      // this is usually the encoding and we do not want encoding=""
      // to show up
      DeleteEmptyAttributes;
      Line := Indent + '<?xml' + WriteInnerTag + '?>';
    end;
  xeStylesheet: // Stylesheet <?xml-stylesheet{stylesheet}?>
    Line := Indent + '<?xml-stylesheet' + WriteInnerTag + '?>';
  xeDoctype:
    begin
      if NodeCount = 0 then
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
      end;
    end;
  xeElement:
    Line := Indent + '<!ELEMENT ' + Name + ' ' + ValueDirect + '>';
  xeAttList:
    Line := Indent + '<!ATTLIST ' + Name + ' ' + ValueDirect + '>';
  xeEntity:
    Line := Indent + '<!ENTITY ' + Name + ' ' + ValueDirect + '>';
  xeNotation:
    Line := Indent + '<!NOTATION ' + Name + ' ' + ValueDirect + '>';
  xeComment: // Comment <!--{comment}-->
    Line := Indent + '<!--' + ValueDirect + '-->';
  xeCData: // literal data <![CDATA[{data}]]>
    Line := Indent + '<![CDATA[' + ValueDirect + ']]>';
  xeExclam: // Any <!data>
    Line := Indent + '<!' + ValueDirect + '>';
  xeQuestion: // Any <?data?>
    Line := Indent + '<?' + ValueDirect + '?>';
  xeCharData:
    Line := FValue;
  xeUnknown: // Any <data>
    Line := Indent + '<' + ValueDirect + '>';
  xeNormal: // normal nodes (xeNormal)
    begin
      // Write tag
      Line := Line + '<' + FName + WriteInnerTag + '>';

      // Write value (if any)
      Line := Line + FValue;
      if (NodeCount > 0) then
        // ..and a linefeed
        Line := Line + LFeed;

      sdUTF8WriteStringToStream(S, Line);

      // Write child elements
      for i := 0 to NodeCount - 1 do
      begin
        ThisNode := Nodes[i];
        NextNode := Nodes[i + 1];
        ThisNode.WriteToStream(S);
        AddLineFeed := True;
        if ThisNode.ElementType = xeCharData then
          AddLineFeed := False;
        if assigned(NextNode) then
          if NextNode.ElementType = xeCharData then
            AddLineFeed := False;
        if AddLineFeed then
          sdUTF8WriteStringToStream(S, LFeed);
      end;

      // Write end tag
      Line := '';
      if not QualifyAsDirectNode then
      begin
        if NodeCount > 0 then
          Line := Indent;
        Line := Line + '</' + FName + '>';
      end;
    end;
  else
    raise EFilerError.Create(sxeIllegalElementType);
  end;//case
  sdUTF8WriteStringToStream(S, Line);

  // Call the onprogress
  if assigned(Document) then
    Document.DoProgress(S.Position);
end;

function TXmlNode.WriteToString: UTF8String;
var
  S: TsdUTF8StringStream;
begin
  // We will simply call WriteToStream and collect the result as UTF8String using
  // a string stream
  S := TsdUTF8StringStream.Create('');
  try
    WriteToStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TXmlNode.WriteUnicodeString(const AName: UTF8String;
  const AValue: UnicodeString; const ADefault: UnicodeString);
begin
  WriteString(AName, FromUnicodeString(AValue), FromUnicodeString(ADefault));
end;

{ TXmlNodeList }

function TXmlNodeList.ByAttribute(const AName, AValue: UTF8String): TXmlNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if UTF8CompareText(Items[i].AttributeByName[AName], AValue) = 0 then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TXmlNodeList.GetItems(Index: Integer): TXmlNode;
begin
  Result := TXmlNode(Get(Index));
end;

procedure TXmlNodeList.SetItems(Index: Integer; const Value: TXmlNode);
begin
  Put(Index, TPointer(Value));
end;

{ TRelaxXml }

procedure TRelaxXml.Assign(Source: TPersistent);
  // local
  procedure SetDocumentRecursively(ANode: TXmlNode; ADocument: TRelaxXml);
  var
    i: integer;
  begin
    ANode.Document := ADocument;
    for i := 0 to ANode.NodeCount - 1 do
      SetDocumentRecursively(ANode.Nodes[i], ADocument);
  end;
// main
begin
  if Source is TRelaxXml then
  begin
    // Copy private members
    FBinaryEncoding := TRelaxXml(Source).FBinaryEncoding;
    FDropCommentsOnParse := TRelaxXml(Source).FDropCommentsOnParse;
    FExternalEncoding := TRelaxXml(Source).FExternalEncoding;
    FParserWarnings := TRelaxXml(Source).FParserWarnings;
    FIndentString := TRelaxXml(Source).FIndentString;
    FUseFullNodes := TRelaxXml(Source).FUseFullNodes;
    FUseLocalBias := TRelaxXml(Source).FUseLocalBias;
    FWriteOnDefault := TRelaxXml(Source).FWriteOnDefault;
    FXmlFormat := TRelaxXml(Source).FXmlFormat;
    // Assign root
    FRootNodes.Assign(TRelaxXml(Source).FRootNodes);
    // Set Document property recursively
    SetDocumentRecursively(FRootNodes, Self);
  end else
    if Source is TXmlNode then
    begin
      // Assign this node to the FRootNodes property
      FRootNodes.Assign(Source);
      // Set Document property recursively
      SetDocumentRecursively(FRootNodes, Self);
    end else
      inherited;
end;

procedure TRelaxXml.Clear;
var
  Node: TXmlNode;
begin
  // Reset defaults
  SetDefaults;
  // Clear root
  FRootNodes.Clear;
  // Build default items in RootNodes
  // - first the declaration
  Node := TXmlNode.CreateType(Self, xeDeclaration);
  Node.Name := 'xml';
  Node.AttributeAdd('version', cDefaultVersionString);
  Node.AttributeAdd('encoding', cDefaultEncodingString);
  FRootNodes.NodeAdd(Node);
  // - then the root node
  FRootNodes.NodeNew('');
end;

procedure TRelaxXml.CopyFrom(Source: TRelaxXml);
begin
  if not assigned(Source) then
    exit;
  Assign(Source);
end;

constructor TRelaxXml.Create;
begin
  inherited Create;
  FRootNodes := TXmlNode.Create(Self);
  Clear;
end;

constructor TRelaxXml.CreateName(const ARootName: UTF8String);
begin
  Create;
  Root.Name := ARootName;
end;

constructor TRelaxXml.New;
begin
  CreateName('xml');
end;

destructor TRelaxXml.Destroy;
begin
  FreeAndNil(FRootNodes);
  inherited;
end;

procedure TRelaxXml.Canonicalize;
begin
// todo
end;
procedure TRelaxXml.DoNodeLoaded(Node: TXmlNode);
begin
  if assigned(FOnNodeLoaded) then
    FOnNodeLoaded(Self, Node);
end;

procedure TRelaxXml.DoNodeNew(Node: TXmlNode);
begin
  if assigned(FOnNodeNew) then
    FOnNodeNew(Self, Node);
end;

procedure TRelaxXml.DoProgress(Size: integer);
begin
  if assigned(FOnProgress) then
    FOnProgress(Self, Size);
end;

procedure TRelaxXml.DoUnicodeLoss(Sender: TObject);
begin
  if assigned(FOnUnicodeLoss) then
    FOnUnicodeLoss(Self);
end;

function TRelaxXml.GetCommentString: UTF8String;
// Get the first comment node, and return its value
var
  Node: TXmlNode;
begin
  Result := '';
  Node := FRootNodes.NodeByElementType(xeComment);
  if assigned(Node) then
    Result := Node.ValueAsString;
end;

function TRelaxXml.GetCharSet: UTF8String;
begin
  Result := '';
  if FRootNodes.NodeCount > 0 then
    if FRootNodes[0].ElementType = xeDeclaration then
      Result := FRootNodes[0].AttributeByName['encoding'];
end;

function TRelaxXml.GetEntityByName(AName: UTF8String): UTF8String;
var
  i, j: integer;
begin
  Result := '';
  for i := 0 to FRootNodes.NodeCount - 1 do
    if FRootNodes[i].ElementType = xeDoctype then with FRootNodes[i] do
    begin
      for j := 0 to NodeCount - 1 do
        if (Nodes[j].ElementType = xeEntity) and (Nodes[j].Name = AName) then
        begin
          Result := sdUTF8UnQuotedString(sdUTF8Trim(Nodes[j].ValueDirect));
          exit;
        end;
    end;
end;

function TRelaxXml.GetRoot: TXmlNode;
begin
  Result := FRootNodes.NodeByElementType(xeNormal);
end;

function TRelaxXml.GetStyleSheetNode: TXmlNode;
begin
  Result := FRootNodes.NodeByElementType(xeStylesheet);
  if not assigned(Result) then
  begin
    // Add a stylesheet node as second one if none present
    Result := TXmlNode.CreateType(Self, xeStyleSheet);
    FRootNodes.NodeInsert(1, Result);
  end;
end;

function TRelaxXml.GetUtf8Encoded: boolean;
begin
  Result := True;
end;

function TRelaxXml.GetVersionString: UTF8String;
begin
  Result := '';
  if FRootNodes.NodeCount > 0 then
    if FRootNodes[0].ElementType = xeDeclaration then
      Result := FRootNodes[0].AttributeByName['version'];
end;

function TRelaxXml.IsEmpty: boolean;
var
  R: TXmlNode;
begin
  Result := True;
  R := GetRoot;
  if assigned(R) then
    Result := R.IsClear;
end;

function TRelaxXml.LineFeed: UTF8String;
begin
  case XmlFormat of
  xfReadable:
    Result := #13#10;
  xfCompact:
    Result := #10;
  else
    Result := #10;
  end;//case
end;

procedure TRelaxXml.LoadFromFile(const AFileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRelaxXml.LoadFromStream(Stream: TStream);
var
  B: TsdBufferedReadStream;
begin
  // Create buffer filter. Since we read from the original stream a buffer at a
  // time, this speeds up the reading process for disk-based files.
  B := TsdBufferedReadStream.Create(Stream, False);
  try
    // We will create a conversion stream as intermediate
    FCodecStream := TsdUtf8Stream.Create(B);
    try
      // Connect events
      FCodecStream.OnUnicodeLoss := DoUnicodeLoss;
      // Read from stream
      ReadFromStream(FCodecStream);
      // Set our external encoding
      FExternalEncoding := FCodecStream.Encoding;
    finally
      FreeAndNil(FCodecStream);
    end;
  finally
    B.Free;
  end;
end;

procedure TRelaxXml.ParseDTD(ANode: TXmlNode; S: TStream);
// DTD parsing is quite different from normal node parsing so it is brought
// under in the main NativeXml object
  // local
  procedure ParseMarkupDeclarations;
  var
    Ch: AnsiChar;
  begin
    repeat
      ANode.NodeNew('').ReadFromStream(S);
      // Read AnsiCharacter, exit if none available
      repeat
        if S.Read(Ch, 1) = 0 then
          exit;
      // Read until end markup declaration or end
      until not (Ch in cControlChars);
      if Ch = ']' then
        break;
      S.Seek(-1, soCurrent);
    until False;
  end;
// main
var
  Prework: UTF8String;
  Ch: AnsiChar;
  Words: TsdUTF8StringList;
begin
  // Get the name and external ID
  Prework := '';
  repeat
    // Read AnsiCharacter, exit if none available
    if S.Read(Ch, 1) = 0 then
      exit;
    // Read until markup declaration or end
    if Ch in ['[', '>'] then
      break;
    Prework := Prework + UTF8String(Ch);
  until False;
  Words := TsdUTF8StringList.Create;
  try
    sdUTF8ParseAttributes(Prework, 1, length(Prework) + 1, Words);
    // First word is name
    if Words.Count > 0 then
    begin
      ANode.Name := Words[0];
      Words.Delete(0);
      // Put the rest in the valuedirect
      ANode.ValueDirect := sdUTF8Trim(sdUTF8StringReplace(Words.Text, #13#10, ' '));
    end;
  finally
    Words.Free;
  end;

  if Ch = '[' then
  begin

    // Parse any !ENTITY nodes and such
    ParseMarkupDeclarations;

    // read final tag
    repeat
      if S.Read(Ch, 1) = 0 then
        exit;
      if Ch = '>' then
        break;
    until False;

  end;
end;

procedure TRelaxXml.ReadFromStream(S: TStream);
var
  i: integer;
  Node: TXmlNode;
  Enc: UTF8String;
  NormalCount, DeclarationCount,
  DoctypeCount, CDataCount: integer;
  NormalPos, DoctypePos: integer;
begin
  FAbortParsing := False;
  with FRootNodes do
  begin
    // Clear the old root nodes - we do not reset the defaults
    Clear;
    DoProgress(0);
    repeat
      Node := NodeNew('');
      Node.ReadFromStream(S);
      if AbortParsing then
        exit;

      // XML declaration
      if Node.ElementType = xeDeclaration then
      begin
        if Node.HasAttribute('encoding') then
          Enc := Node.AttributeByName['encoding']
        else
          FCodecStream.Encoding := seUTF8;
        // Check encoding
        if assigned(FCodecStream) and (AnsiUpperCase(string(Enc)) = 'UTF-8') then
          FCodecStream.Encoding := seUTF8;
      end;
      // Skip clear nodes
      if Node.IsClear then
        NodeDelete(NodeCount - 1);
    until S.Position >= S.Size;
    DoProgress(S.Size);

    // Do some checks
    NormalCount      := 0;
    DeclarationCount := 0;
    DoctypeCount     := 0;
    CDataCount       := 0;
    NormalPos        := -1;
    DoctypePos       := -1;
    for i := 0 to NodeCount - 1 do
    begin
      // Count normal elements - there may be only one
      case Nodes[i].ElementType of
      xeNormal:
        begin
          inc(NormalCount);
          NormalPos := i;
        end;
      xeDeclaration: inc(DeclarationCount);
      xeDoctype:
        begin
          inc(DoctypeCount);
          DoctypePos := i;
        end;
      xeCData: inc(CDataCount);
      end;//case
    end;

    // We *must* have a root node
    if NormalCount = 0 then
      raise EFilerError.Create(sxeNoRootElement);

    // Do some validation if we allow parser warnings
    if FParserWarnings then
    begin

      // Check for more than one root node
      if NormalCount > 1 then
        raise EFilerError.Create(sxeMoreThanOneRootElement);

      // Check for more than one xml declaration
      if DeclarationCount > 1 then
        raise EFilerError.Create(sxeMoreThanOneDeclaration);

      // Declaration must be first element if present
      if DeclarationCount = 1 then
        if Nodes[0].ElementType <> xeDeclaration then
          raise EFilerError.Create(sxeDeclarationMustBeFirstElem);

      // Check for more than one DTD
      if DoctypeCount > 1 then
        raise EFilerError.Create(sxeMoreThanOneDoctype);

      // Check if DTD is after root, this is not allowed
      if (DoctypeCount = 1) and (DoctypePos > NormalPos) then
        raise EFilerError.Create(sxeDoctypeAfterRootElement);

      // No CDATA in root allowed
      if CDataCount > 0 then
        raise EFilerError.Create(sxeCDataInRoot);
    end;
  end;//with
end;

procedure TRelaxXml.ReadFromString(const AValue: UTF8String);
var
  S: TStream;
begin
  S := TsdUTF8StringStream.Create(AValue);
  try
    ReadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRelaxXml.ResolveEntityReferences;
begin
  if assigned(Root) then
    Root.ResolveEntityReferences;
end;

procedure TRelaxXml.SaveToFile(const AFileName: string);
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

procedure TRelaxXml.SaveToStream(Stream: TStream);
var
  B: TsdBufferedWriteStream;
begin
  // Create buffer filter. Since we write a buffer at a time to the destination
  // stream, this speeds up the writing process for disk-based files.
  B := TsdBufferedWriteStream.Create(Stream, False);
  try
    // Create conversion stream
    FCodecStream := TsdUtf8Stream.Create(B);
    try
      // Set External encoding
      FCodecStream.Encoding := FExternalEncoding;
      WriteToStream(FCodecStream);
    finally
      FreeAndNil(FCodecStream);
    end;
  finally
    B.Free;
  end;
end;

procedure TRelaxXml.SetCommentString(const Value: UTF8String);
// Find first comment node and set it's value, otherwise add new comment node
// right below the xml declaration
var
  Node: TXmlNode;
begin
  Node := FRootNodes.NodeByElementType(xeComment);
  if not assigned(Node) and (length(Value) > 0) then
  begin
    Node := TXmlNode.CreateType(Self, xeComment);
    FRootNodes.NodeInsert(1, Node);
  end;
  if assigned(Node) then
    Node.ValueAsString := Value;
end;

procedure TRelaxXml.SetDefaults;
begin
  // Defaults
  FExternalEncoding       := cDefaultExternalEncoding;
  FXmlFormat              := cDefaultXmlFormat;
  FWriteOnDefault         := cDefaultWriteOnDefault;
  FBinaryEncoding         := cDefaultBinaryEncoding;
  FIndentString           := cDefaultIndentString;
  FDropCommentsOnParse    := cDefaultDropCommentsOnParse;
  FUseFullNodes           := cDefaultUseFullNodes;
  FUseLocalBias           := cDefaultUseLocalBias;
  FFloatAllowScientific   := cDefaultFloatAllowScientific;
  FFloatSignificantDigits := cDefaultFloatSignificantDigits;
  FOnNodeNew              := nil;
  FOnNodeLoaded           := nil;
end;

procedure TRelaxXml.SetCharSet(const Value: UTF8String);
var
  Node: TXmlNode;
begin
  if Value = GetCharSet then
    exit;
  Node := FRootNodes[0];
  if not assigned(Node) or (Node.ElementType <> xeDeclaration) then
  begin
    Node := TXmlNode.CreateType(Self, xeDeclaration);
    FRootNodes.NodeInsert(0, Node);
  end;
  if assigned(Node) then
    Node.AttributeByName['encoding'] := Value;
end;

procedure TRelaxXml.SetVersionString(const Value: UTF8String);
var
  Node: TXmlNode;
begin
  if Value = GetVersionString then
    exit;
  Node := FRootNodes[0];
  if not assigned(Node) or (Node.ElementType <> xeDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TXmlNode.CreateType(Self, xeDeclaration);
      FRootNodes.NodeInsert(0, Node);
    end;
  end;
  if assigned(Node) then
    Node.AttributeByName['version'] := Value;
end;

procedure TRelaxXml.WriteToStream(S: TStream);
var
  i: integer;
begin
  if not assigned(Root) and FParserWarnings then
    raise EFilerError.Create(sxeRootElementNotDefined);

  DoProgress(0);

  // write the root nodes
  for i := 0 to FRootNodes.NodeCount - 1 do
  begin
    FRootNodes[i].WriteToStream(S);
    sdUTF8WriteStringToStream(S, LineFeed);
  end;

  DoProgress(S.Size);
end;

function TRelaxXml.WriteToString: UTF8String;
var
  S: TsdUTF8StringStream;
begin
  S := TsdUTF8StringStream.Create('');
  try
    WriteToStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

{ TsdCodecStream }

constructor TsdCodecStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TsdCodecStream.InternalRead(var Buffer{$IFDEF CLR}: array of Byte{$ENDIF}; Offset, Count: Longint): Longint;
// Read from FStream and pass back data
var
  i, j: integer;
  BOM: array[0..3] of byte;
  BytesRead: integer;
  Found: boolean;
begin
  Result := 0;
  if FMode = umUnknown then
  begin
    FMode := umRead;
    // Check FStream
    if not assigned(FStream) then
      raise EStreamError.Create(sxeCodecStreamNotAssigned);

    // Determine encoding
    FEncoding := seAnsi;
    BytesRead := FStream.Read(BOM, 4);
    for i := 0 to cBomInfoCount - 1 do
    begin
      Found := True;
      for j := 0 to Min(BytesRead, cBomInfo[i].Len) - 1 do
      begin
        if BOM[j] <> cBomInfo[i].BOM[j] then
        begin
          Found := False;
          break;
        end;
      end;
      if Found then
        break;
    end;
    if Found then
    begin
      FEncoding := cBomInfo[i].Encoding;
      FWriteBom := cBomInfo[i].HasBOM;
    end else
    begin
      // Unknown.. default to this
      FEncoding := seAnsi;
      FWriteBom := False;
    end;

    // Some encodings are not supported (yet)
    if FEncoding in [seUCS4BE, seUCS4_2143, seUCS4_3412, seEBCDIC] then
      raise EStreamError.Create(sxeUnsupportedEncoding);

    // Correct stream to start position
    if FWriteBom then
      FStream.Seek(cBomInfo[i].Len - BytesRead, soCurrent)
    else
      FStream.Seek(-BytesRead, soCurrent);

    // Check if we must swap byte order
    if FEncoding in [se16BitBE, seUTF16BE] then
      FSwapByteOrder := True;

  end;

  // Check mode
  if FMode <> umRead then
    raise EStreamError.Create(sxeCannotReadCodecForWriting);

  // Check count
  if Count <> 1 then
    raise EStreamError.Create(sxeCannotReadMultipeChar);

  // Now finally read
{  TBytes(Buffer)[Offset] := ReadByte;  todo
  if TBytes(Buffer)[Offset] <> 0 then Result := 1; }
end;


function TsdCodecStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := InternalRead(Buffer, 0, Count);
end;

function TsdCodecStream.ReadByte: byte;
begin
  // default does nothing
  Result := 0;
end;

function TsdCodecStream.InternalSeek(Offset: Longint; Origin: TSeekOrigin): Longint;
begin
  Result := 0;
  if FMode = umUnknown then
    raise EStreamError.Create(sxeCannotSeekBeforeReadWrite);

  if Origin = soCurrent then
  begin
    if Offset = 0 then
    begin
      // Position
      Result := FStream.Position;
      exit;
    end;
    if (FMode = umRead) and ((Offset = -1) or (Offset = -2)) then
    begin
      FBuffer := '';
      case Offset of
      -1: FStream.Seek(FPosMin1, soBeginning);
      -2: FStream.Seek(FPosMin2, soBeginning);
      end;//case
      exit;
    end;
  end;
  if (Origin = soEnd) and (Offset = 0) then
  begin
    // Size
    Result := FStream.Size;
    exit;
  end;
  // Ignore set position from beginning (used in Size command)
  if Origin = soBeginning then
    exit;
  // Arriving here means we cannot do it
  raise EStreamError.Create(sxeCannotPerformSeek);
end;

function TsdCodecStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := InternalSeek(Offset, TSeekOrigin(Origin));
end;

procedure TsdCodecStream.StorePrevPositions;
begin
  FPosMin2 := FPosMin1;
  FPosMin1 := FStream.Position;
end;

function TsdCodecStream.InternalWrite(const Buffer; Offset, Count: Longint): Longint;
var
  i: integer;
begin
  if FMode = umUnknown then
  begin
    FMode := umWrite;

    // Some encodings are not supported (yet)
    if FEncoding in [seUCS4BE, seUCS4_2143, seUCS4_3412, seEBCDIC] then
      raise EStreamError.Create(sxeUnsupportedEncoding);

    // Find correct encoding info
    for i := 0 to cBomInfoCount - 1 do
      if cBomInfo[i].Encoding = FEncoding then
      begin
        // we do not write BOM if UTF8 since UTF8 is default
        FWriteBom := cBomInfo[i].HasBOM and (FEncoding <>  seUTF8);
        break;
      end;

    // Write BOM
    if FWriteBom then
      FStream.WriteBuffer(cBomInfo[i].BOM, cBomInfo[i].Len);

    // Check if we must swap byte order
    if FEncoding in [se16BitBE, seUTF16BE] then
      FSwapByteOrder := True;
  end;

  if FMode <> umWrite then
    raise EStreamError.Create(sxeCannotWriteCodecForReading);
  WriteBuf(Buffer, Offset, Count);
  Result := Count;
end;


function TsdCodecStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := InternalWrite(Byte(Buffer), 0, Count);
end;

procedure TsdCodecStream.WriteBuf(const Buffer; Offset, Count: longint);
var
  i: integer;
begin
  // Default just writes out bytes one by one. We override this in descendants
  // to provide faster writes for some modes
  for i := 0 to Count - 1 do
//    WriteByte(TBytes(Buffer)[Offset + i]); todo
end;

procedure TsdCodecStream.WriteByte(const B: byte);
begin
// default does nothing
end;


{ TsdUtf8Stream }

function TsdUtf8Stream.ReadByte: byte;
var
  B, B1, B2, B3: byte;
  W: word;
  SA: AnsiString;
begin
  Result := 0;

  // New AnsiCharacter?
  if (Length(FBuffer) = 0) or (FBufferPos > length(FBuffer)) then
  begin
    StorePrevPositions;
    FBufferPos := 1;
    // Read another AnsiChar and put in buffer
    case FEncoding of
    seAnsi:
      begin
        // read one byte
        B := 0;
        FStream.Read(B, 1);
        SA := AnsiChar(B);
        // Convert to UTF8
        FBuffer := sdAnsiToUtf8(SA);
      end;
    seUTF8:
      begin
        // Read one, two or three bytes in the buffer
        B1 := 0;
        FStream.Read(B1, 1);
        FBuffer := AnsiChar(B1);
        if (B1 and $80) > 0 then
        begin
          if (B1 and $20) <> 0 then
          begin
            B2 := 0;
            FStream.Read(B2, 1);
            FBuffer := FBuffer + UTF8String(AnsiChar(B2));
          end;
          B3 := 0;
          FStream.Read(B3, 1);
          FBuffer := FBuffer + UTF8String(AnsiChar(B3));
        end;
      end;
    se16BitBE, se16BitLE, seUTF16BE, seUTF16LE:
      begin
        // Read two bytes
        W := 0;
        FStream.Read(W, 2);
        // Swap byte order
        if FSwapByteOrder then
          W := swap(W);
        // Convert to UTF8 in buffer
        FBuffer := sdUnicodeToUtf8(UnicodeChar(W));
      end;
    else
      raise EStreamError.Create(sxeUnsupportedEncoding);
    end;//case
  end;

  // Now we have the buffer, so read
  if (FBufferPos > 0) and (FBufferPos <= length(FBuffer)) then
    Result := byte(FBuffer[FBufferPos]);
  inc(FBufferPos);
end;

procedure TsdUtf8Stream.WriteBuf(const Buffer; Offset, Count: longint);
begin
  case FEncoding of
  seUtf8:
    begin
      // one on one
      if StreamWrite(FStream, Buffer, Offset, Count) <> Count then
        raise EStreamError.Create(sxeCannotWriteToOutputStream);
    end
  else
    inherited;
  end;//case
end;

procedure TsdUtf8Stream.WriteByte(const B: byte);
var
  SA: AnsiString;
  SW: UnicodeString;
  MustWrite: boolean;
begin
  case FEncoding of
  seAnsi, se16BitBE, se16BitLE, seUTF16BE, seUTF16LE:
    begin
      MustWrite := True;
      case Length(FBuffer) of
      0:
        begin
          FBuffer := AnsiChar(B);
          if (B and $80) <> 0 then
            MustWrite := False;
        end;
      1:
        begin
          FBuffer := FBuffer + UTF8String(AnsiChar(B));
          if (byte(FBuffer[1]) and $20) <> 0 then
            MustWrite := False;
        end;
      2: FBuffer := FBuffer + UTF8String(AnsiChar(B));
      end;
      if MustWrite then
      begin
        if FEncoding = seAnsi then
        begin
          // Convert to ansi
          SA := sdUtf8ToAnsi(FBuffer);
          // write out
          if length(SA) = 1 then
            if FStream.Write(SA[1], 1) <> 1 then
              raise EStreamError.Create(sxeCannotWriteToOutputStream);
        end else
        begin
          // Convert to unicode
          SW := sdUtf8ToUnicode(FBuffer);
          // write out
          if length(SW) = 1 then
            if FStream.Write(SW[1], 2) <> 2 then
              raise EStreamError.Create(sxeCannotWriteToOutputStream);
        end;
        FBuffer := '';
      end;
    end;
  seUTF8:
    begin
      // Just a flat write of one byte
      if FStream.Write(B, 1) <> 1 then
        raise EStreamError.Create(sxeCannotWriteToOutputStream);
    end;
  else
    raise EStreamError.Create(sxeUnsupportedEncoding);
  end;//case
end;


{ TsdBufferedReadStream }

const
  cMaxBufferSize = $10000; // 65536 bytes in the buffer

procedure TsdBufferedReadStream.CheckPosition;
var
  NewPage: integer;
  FStartPos: longint;
begin
  // Page and buffer position
  NewPage := FPosition div cMaxBufferSize;
  FBufPos := FPosition mod cMaxBufferSize;

  // Read new page if required
  if (NewPage <> FPage) then
  begin
    // New page and buffer
    FPage := NewPage;

    // Start position in stream
    FStartPos := FPage * cMaxBufferSize;
    FBufSize  := Min(cMaxBufferSize, FStream.Size - FStartPos);

    FStream.Seek(FStartPos, soBeginning);
    if FBufSize > 0 then
      FStream.Read(FBuffer^, FBufSize);
  end;
  FMustCheck := False;
end;

constructor TsdBufferedReadStream.Create(AStream: TStream; Owned: boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwned := Owned;
  FMustCheck := True;
  FPage := -1; // Set to invalid number to force an update on first read
  ReallocMem(FBuffer, cMaxBufferSize);
end;

destructor TsdBufferedReadStream.Destroy;
begin
  if FOwned then FreeAndNil(FStream);
  ReallocMem(FBuffer, 0);
  inherited;
end;

function TsdBufferedReadStream.Read(var Buffer; Count: longint): Longint;
var
  Packet: PByte;
  PacketCount: integer;
begin
  // Set the right page
  if FMustCheck then
    CheckPosition;

  // Special case - read one byte, most often
  if (Count = 1) and (FBufPos < FBufSize - 1) then
  begin
    byte(Buffer) := FBuffer^[FBufPos];
    inc(FBufPos);
    inc(FPosition);
    Result := 1;
    exit;
  end;

  // general case
  Packet := @Buffer;
  Result := 0;
  while Count > 0 do
  begin
    PacketCount := min(FBufSize - FBufPos, Count);
    if PacketCount <= 0 then
      exit;
    Move(FBuffer^[FBufPos], Packet^, PacketCount);
    dec(Count, PacketCount);
    inc(Packet, PacketCount);
    inc(Result, PacketCount);
    inc(FPosition, PacketCount);
    inc(FBufPos, PacketCount);
    if FBufPos >= FBufSize then
      CheckPosition;
  end;
end;

function TsdBufferedReadStream.Seek(Offset: longint; Origin: Word): Longint;
begin
  case Origin of
  soFromBeginning:
    FPosition := Offset;
  soFromCurrent:
    begin
      // no need to check in this case - it is the GetPosition command
      if Offset = 0 then
      begin
        Result := FPosition;
        exit;
      end;
      FPosition := FPosition + Offset;
    end;
  soFromEnd:
    FPosition := FStream.Size + Offset;
  end;//case
  Result := FPosition;
  FMustCheck := True;
end;

function TsdBufferedReadStream.Write(const Buffer; Count: longint): Longint;
begin
  raise EStreamError.Create(sxeCannotWriteCodecForReading);
end;

{ TsdBufferedWriteStream }

constructor TsdBufferedWriteStream.Create(AStream: TStream; Owned: boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwned := Owned;
  ReallocMem(FBuffer, cMaxBufferSize);
end;

destructor TsdBufferedWriteStream.Destroy;
begin
  Flush;
  if FOwned then
    FreeAndNil(FStream);
  ReallocMem(FBuffer, 0);
  inherited;
end;

procedure TsdBufferedWriteStream.Flush;
begin
  // Write the buffer to the stream
  if FBufPos > 0 then
  begin
    FStream.Write(FBuffer^, FBufPos);
    FBufPos := 0;
  end;
end;

function TsdBufferedWriteStream.Read(var Buffer; Count: longint): Longint;
begin
  raise EStreamError.Create(sxeCannotReadCodecForWriting);
end;

function TsdBufferedWriteStream.Seek(Offset: longint; Origin: Word): Longint;
begin
  case Origin of
  soFromBeginning:
    if Offset = FPosition then
    begin
      Result := FPosition;
      exit;
    end;
  soFromCurrent:
    begin
      // GetPosition command
      if Offset = 0 then
      begin
        Result := FPosition;
        exit;
      end;
    end;
  soFromEnd:
    if Offset = 0 then
    begin
      Result := FPosition;
      exit;
    end;
  end;//case
  raise EStreamError.Create(sxeCannotPerformSeek);
end;

function TsdBufferedWriteStream.Write(const Buffer; Count: longint): Longint;
var
  Packet: PByte;
  PacketCount: integer;
begin
  // Special case - read less bytes than would fill buffersize
  if (FBufPos + Count < cMaxBufferSize) then
  begin
    Move(Buffer, FBuffer^[FBufPos], Count);
    inc(FBufPos, Count);
    inc(FPosition, Count);
    Result := Count;
    exit;
  end;

  // general case that wraps buffer
  Packet := @Buffer;
  Result := 0;
  while Count > 0 do
  begin
    PacketCount := min(cMaxBufferSize - FBufPos, Count);
    if PacketCount <= 0 then
      exit;
    Move(Packet^, FBuffer^[FBufPos], PacketCount);
    dec(Count,     PacketCount);
    inc(Result,    PacketCount);
    inc(FPosition, PacketCount);
    inc(Packet,    PacketCount);
    inc(FBufPos,   PacketCount);
    if FBufPos = cMaxBufferSize then
      Flush;
  end;
end;

{ TsdSurplusReader }

constructor TsdSurplusReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TsdSurplusReader.ReadChar(var Ch: AnsiChar): integer;
begin
  if length(FSurplus) > 0 then
  begin
    Ch := FSurplus[1];
    FSurplus := copy(FSurplus, 2, length(FSurplus) - 1);
    Result := 1;
  end else
    Result := FStream.Read(Ch, 1);
end;

function TsdSurplusReader.ReadCharSkipBlanks(var Ch: AnsiChar): boolean;
begin
  Result := False;
  repeat
    // Read AnsiCharacter, exit if none available
    if ReadChar(Ch) = 0 then
      exit;
    // Skip if in controlchars
    if not (Ch in cControlchars) then
      break;
  until False;
  Result := True;
end;

{ TsdStringBuilder }

procedure TsdStringBuilder.AddChar(Ch: AnsiChar);
begin
  inc(FCurrentIdx);
  Reallocate(FCurrentIdx);
  FData[FCurrentIdx] := Ch;
end;

procedure TsdStringBuilder.AddString(var S: UTF8String);
var
  Count: integer;
begin
  Count := System.length(S);
  if Count = 0 then
    exit;
  Reallocate(FCurrentIdx + Count);
  Move(S[1], FData[FCurrentIdx + 1], Count);
  inc(FCurrentIdx, Count);
end;

procedure TsdStringBuilder.Clear;
begin
  FCurrentIdx := 0;
end;

function TsdStringBuilder.StringCopy(AFirst, ALength: integer): UTF8String;
begin
  if ALength > FCurrentIdx - AFirst + 1 then
    ALength := FCurrentIdx - AFirst + 1;
  Result := Copy(FData, AFirst, ALength);
end;

constructor TsdStringBuilder.Create;
begin
  inherited Create;
  SetLength(FData, 64);
end;

function TsdStringBuilder.GetData(Index: integer): AnsiChar;
begin
  Result := FData[Index];
end;

procedure TsdStringBuilder.Reallocate(RequiredLength: integer);
begin
  while System.Length(FData) < RequiredLength do
    SetLength(FData, System.Length(FData) * 2);
end;

function TsdStringBuilder.Value: UTF8String;
begin
  Result := Copy(FData, 1, FCurrentIdx);
end;

end.
