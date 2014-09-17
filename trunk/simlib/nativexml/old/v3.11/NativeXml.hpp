// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Nativexml.pas' rev: 10.00

#ifndef NativexmlHPP
#define NativexmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Nativexml
{
//-- type declarations -------------------------------------------------------
typedef void *TPointer;

#pragma option push -b-
enum TXmlFormatType { xfReadable, xfCompact };
#pragma option pop

#pragma option push -b-
enum TXmlElementType { xeNormal, xeComment, xeCData, xeDeclaration, xeStylesheet, xeDoctype, xeElement, xeAttList, xeEntity, xeNotation, xeExclam, xeQuestion, xeCharData, xeUnknown };
#pragma option pop

#pragma option push -b-
enum TBinaryEncodingType { xbeBinHex, xbeBase64 };
#pragma option pop

#pragma option push -b-
enum TStringEncodingType { se8Bit, seUCS4BE, seUCS4LE, seUCS4_2143, seUCS4_3412, se16BitBE, se16BitLE, seUTF8, seUTF16BE, seUTF16LE, seEBCDIC };
#pragma option pop

#pragma option push -b-
enum TXmlCompareOption { xcNodeName, xcNodeType, xcNodeValue, xcAttribCount, xcAttribNames, xcAttribValues, xcChildCount, xcChildNames, xcChildValues, xcRecursive };
#pragma option pop

typedef Set<TXmlCompareOption, xcNodeName, xcRecursive>  TXmlCompareOptions;

class DELPHICLASS TXmlNode;
typedef void __fastcall (__closure *TXmlNodeEvent)(System::TObject* Sender, TXmlNode* Node);

typedef void __fastcall (__closure *TXmlProgressEvent)(System::TObject* Sender, int Size);

typedef int __fastcall (__closure *TXmlNodeCompareEvent)(System::TObject* Sender, TXmlNode* Node1, TXmlNode* Node2, void * Info);

typedef int __fastcall (*TXMLNodeCompareFunction)(TXmlNode* Node1, TXmlNode* Node2, void * Info);

class DELPHICLASS TNativeXml;
class DELPHICLASS TsdCodecStream;
#pragma option push -b-
enum TsdStreamModeType { umUnknown, umRead, umWrite };
#pragma option pop

class PASCALIMPLEMENTATION TsdCodecStream : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	AnsiString FBuffer;
	int FBufferPos;
	TStringEncodingType FEncoding;
	TsdStreamModeType FMode;
	int FPosMin1;
	int FPosMin2;
	Classes::TStream* FStream;
	bool FSwapByteOrder;
	bool FWarningUnicodeLoss;
	bool FWriteBom;
	Classes::TNotifyEvent FOnUnicodeLoss;
	
protected:
	virtual Byte __fastcall ReadByte(void);
	virtual void __fastcall StorePrevPositions(void);
	virtual void __fastcall WriteByte(const Byte B);
	virtual void __fastcall WriteBuf(const void *Buffer, int Offset, int Count);
	int __fastcall InternalRead(void *Buffer, int Offset, int Count);
	int __fastcall InternalSeek(int Offset, Classes::TSeekOrigin Origin);
	int __fastcall InternalWrite(const void *Buffer, int Offset, int Count);
	
public:
	__fastcall virtual TsdCodecStream(Classes::TStream* AStream);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count);
	__property TStringEncodingType Encoding = {read=FEncoding, write=FEncoding, nodefault};
	__property bool WarningUnicodeLoss = {read=FWarningUnicodeLoss, nodefault};
	__property Classes::TNotifyEvent OnUnicodeLoss = {read=FOnUnicodeLoss, write=FOnUnicodeLoss};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TsdCodecStream(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


class PASCALIMPLEMENTATION TNativeXml : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	bool FAbortParsing;
	TBinaryEncodingType FBinaryEncoding;
	TsdCodecStream* FCodecStream;
	bool FDropCommentsOnParse;
	TStringEncodingType FExternalEncoding;
	bool FFloatAllowScientific;
	int FFloatSignificantDigits;
	bool FParserWarnings;
	TXmlNode* FRootNodes;
	AnsiString FIndentString;
	bool FUseFullNodes;
	bool FUtf8Encoded;
	bool FWriteOnDefault;
	TXmlFormatType FXmlFormat;
	bool FSortAttributes;
	TXmlNodeCompareEvent FOnNodeCompare;
	TXmlNodeEvent FOnNodeNew;
	TXmlNodeEvent FOnNodeLoaded;
	TXmlProgressEvent FOnProgress;
	Classes::TNotifyEvent FOnUnicodeLoss;
	void __fastcall DoNodeNew(TXmlNode* Node);
	void __fastcall DoNodeLoaded(TXmlNode* Node);
	void __fastcall DoUnicodeLoss(System::TObject* Sender);
	AnsiString __fastcall GetCommentString();
	void __fastcall SetCommentString(const AnsiString Value);
	AnsiString __fastcall GetEntityByName(AnsiString AName);
	TXmlNode* __fastcall GetRoot(void);
	AnsiString __fastcall GetEncodingString();
	void __fastcall SetEncodingString(const AnsiString Value);
	AnsiString __fastcall GetVersionString();
	void __fastcall SetVersionString(const AnsiString Value);
	TXmlNode* __fastcall GetStyleSheetNode(void);
	
protected:
	virtual void __fastcall CopyFrom(TNativeXml* Source);
	void __fastcall DoProgress(int Size);
	virtual AnsiString __fastcall LineFeed();
	virtual void __fastcall ParseDTD(TXmlNode* ANode, Classes::TStream* S);
	virtual void __fastcall ReadFromStream(Classes::TStream* S);
	virtual void __fastcall WriteToStream(Classes::TStream* S);
	virtual void __fastcall SetDefaults(void);
	
public:
	__fastcall virtual TNativeXml(void);
	__fastcall virtual TNativeXml(const AnsiString ARootName);
	__fastcall virtual ~TNativeXml(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall Clear(void);
	virtual bool __fastcall IsEmpty(void);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	virtual void __fastcall LoadFromFile(const AnsiString FileName);
	virtual void __fastcall ReadFromString(const AnsiString AValue);
	void __fastcall ResolveEntityReferences(void);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall SaveToFile(const AnsiString FileName);
	virtual AnsiString __fastcall WriteToString();
	__property bool AbortParsing = {read=FAbortParsing, write=FAbortParsing, nodefault};
	__property TBinaryEncodingType BinaryEncoding = {read=FBinaryEncoding, write=FBinaryEncoding, nodefault};
	__property AnsiString CommentString = {read=GetCommentString, write=SetCommentString};
	__property bool DropCommentsOnParse = {read=FDropCommentsOnParse, write=FDropCommentsOnParse, nodefault};
	__property AnsiString EncodingString = {read=GetEncodingString, write=SetEncodingString};
	__property AnsiString EntityByName[AnsiString AName] = {read=GetEntityByName};
	__property TStringEncodingType ExternalEncoding = {read=FExternalEncoding, write=FExternalEncoding, nodefault};
	__property bool FloatAllowScientific = {read=FFloatAllowScientific, write=FFloatAllowScientific, nodefault};
	__property int FloatSignificantDigits = {read=FFloatSignificantDigits, write=FFloatSignificantDigits, nodefault};
	__property AnsiString IndentString = {read=FIndentString, write=FIndentString};
	__property TXmlNode* Root = {read=GetRoot};
	__property TXmlNode* RootNodeList = {read=FRootNodes};
	__property TXmlNode* StyleSheetNode = {read=GetStyleSheetNode};
	__property bool UseFullNodes = {read=FUseFullNodes, write=FUseFullNodes, nodefault};
	__property bool Utf8Encoded = {read=FUtf8Encoded, write=FUtf8Encoded, nodefault};
	__property AnsiString VersionString = {read=GetVersionString, write=SetVersionString};
	__property bool WriteOnDefault = {read=FWriteOnDefault, write=FWriteOnDefault, nodefault};
	__property TXmlFormatType XmlFormat = {read=FXmlFormat, write=FXmlFormat, nodefault};
	__property bool ParserWarnings = {read=FParserWarnings, write=FParserWarnings, nodefault};
	__property bool SortAttributes = {read=FSortAttributes, write=FSortAttributes, nodefault};
	__property TXmlNodeCompareEvent OnNodeCompare = {read=FOnNodeCompare, write=FOnNodeCompare};
	__property TXmlNodeEvent OnNodeNew = {read=FOnNodeNew, write=FOnNodeNew};
	__property TXmlNodeEvent OnNodeLoaded = {read=FOnNodeLoaded, write=FOnNodeLoaded};
	__property TXmlProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property Classes::TNotifyEvent OnUnicodeLoss = {read=FOnUnicodeLoss, write=FOnUnicodeLoss};
};


class PASCALIMPLEMENTATION TXmlNode : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
public:
	TXmlNode* operator[](int Index) { return Nodes[Index]; }
	
private:
	Classes::TStringList* FAttributes;
	TNativeXml* FDocument;
	TXmlElementType FElementType;
	AnsiString FName;
	Classes::TList* FNodes;
	TXmlNode* FParent;
	int FTag;
	AnsiString FValue;
	bool __fastcall AbortParsing(void);
	AnsiString __fastcall GetValueAsString();
	void __fastcall SetAttributeName(int Index, const AnsiString Value);
	void __fastcall SetAttributeValue(int Index, const AnsiString Value);
	void __fastcall SetValueAsString(const AnsiString AValue);
	AnsiString __fastcall GetIndent();
	AnsiString __fastcall GetLineFeed();
	int __fastcall GetTreeDepth(void);
	int __fastcall GetAttributeCount(void);
	AnsiString __fastcall GetAttributePair(int Index);
	AnsiString __fastcall GetAttributeName(int Index);
	AnsiString __fastcall GetAttributeValue(int Index);
	bool __fastcall GetWriteOnDefault(void);
	TBinaryEncodingType __fastcall GetBinaryEncoding(void);
	AnsiString __fastcall GetCascadedName();
	bool __fastcall QualifyAsDirectNode(void);
	void __fastcall SetName(const AnsiString Value);
	AnsiString __fastcall GetFullPath();
	void __fastcall SetBinaryEncoding(const TBinaryEncodingType Value);
	AnsiString __fastcall GetBinaryString();
	void __fastcall SetBinaryString(const AnsiString Value);
	bool __fastcall UseFullNodes(void);
	WideString __fastcall GetValueAsWidestring();
	void __fastcall SetValueAsWidestring(const WideString Value);
	AnsiString __fastcall GetAttributeByName(const AnsiString AName);
	void __fastcall SetAttributeByName(const AnsiString AName, const AnsiString Value);
	int __fastcall GetValueAsInteger(void);
	void __fastcall SetValueAsInteger(const int Value);
	double __fastcall GetValueAsFloat(void);
	void __fastcall SetValueAsFloat(const double Value);
	System::TDateTime __fastcall GetValueAsDateTime(void);
	void __fastcall SetValueAsDateTime(const System::TDateTime Value);
	bool __fastcall GetValueAsBool(void);
	void __fastcall SetValueAsBool(const bool Value);
	__int64 __fastcall GetValueAsInt64(void);
	void __fastcall SetValueAsInt64(const __int64 Value);
	void __fastcall CheckCreateAttributesList(void);
	WideString __fastcall GetAttributeValueAsWidestring(int Index);
	void __fastcall SetAttributeValueAsWidestring(int Index, const WideString Value);
	int __fastcall GetAttributeValueAsInteger(int Index);
	void __fastcall SetAttributeValueAsInteger(int Index, const int Value);
	WideString __fastcall GetAttributeByNameWide(const AnsiString AName);
	void __fastcall SetAttributeByNameWide(const AnsiString AName, const WideString Value);
	int __fastcall GetTotalNodeCount(void);
	int __fastcall FloatSignificantDigits(void);
	bool __fastcall FloatAllowScientific(void);
	AnsiString __fastcall GetAttributeValueDirect(int Index);
	void __fastcall SetAttributeValueDirect(int Index, const AnsiString Value);
	
protected:
	int __fastcall CompareNodeName(const AnsiString NodeName);
	void __fastcall DeleteEmptyAttributes(void);
	virtual TXmlNode* __fastcall GetNodes(int Index);
	virtual int __fastcall GetNodeCount(void);
	void __fastcall ParseTag(const AnsiString AValue, int TagStart, int TagClose);
	virtual void __fastcall ReadFromStream(Classes::TStream* S);
	virtual void __fastcall ReadFromString(const AnsiString AValue);
	void __fastcall ResolveEntityReferences(void);
	virtual AnsiString __fastcall UnescapeString(const AnsiString AValue);
	bool __fastcall Utf8Encoded(void);
	virtual AnsiString __fastcall WriteInnerTag();
	virtual void __fastcall WriteToStream(Classes::TStream* S);
	void __fastcall ChangeDocument(TNativeXml* ADocument);
	
public:
	__fastcall virtual TXmlNode(TNativeXml* ADocument);
	__fastcall virtual TXmlNode(TNativeXml* ADocument, const AnsiString AName);
	__fastcall virtual TXmlNode(TNativeXml* ADocument, const AnsiString AName, const AnsiString AValue);
	__fastcall virtual TXmlNode(TNativeXml* ADocument, TXmlElementType AType);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall Delete(void);
	void __fastcall DeleteEmptyNodes(void);
	__fastcall virtual ~TXmlNode(void);
	void __fastcall AttributeAdd(const AnsiString AName, int AValue)/* overload */;
	void __fastcall AttributeAdd(const AnsiString AName, const AnsiString AValue)/* overload */;
	void __fastcall AttributeDelete(int Index);
	void __fastcall AttributeExchange(int Index1, int Index2);
	int __fastcall AttributeIndexByname(const AnsiString AName);
	virtual void __fastcall AttributesClear(void);
	virtual void __fastcall BufferRead(void *Buffer, int Count);
	virtual void __fastcall BufferWrite(const void *Buffer, int Count);
	virtual int __fastcall BufferLength(void);
	virtual void __fastcall Clear(void);
	TXmlNode* __fastcall FindNode(const AnsiString NodeName);
	void __fastcall FindNodes(const AnsiString NodeName, const Classes::TList* AList);
	AnsiString __fastcall FromAnsiString(const AnsiString s);
	AnsiString __fastcall FromWidestring(const WideString W);
	virtual bool __fastcall HasAttribute(const AnsiString AName);
	int __fastcall IndexInParent(void);
	virtual bool __fastcall IsClear(void);
	virtual bool __fastcall IsEmpty(void);
	bool __fastcall IsEqualTo(TXmlNode* ANode, TXmlCompareOptions Options, Classes::TList* MismatchNodes = (Classes::TList*)(0x0));
	virtual int __fastcall NodeAdd(TXmlNode* ANode);
	TXmlNode* __fastcall NodeByAttributeValue(const AnsiString NodeName, const AnsiString AttribName, const AnsiString AttribValue, bool ShouldRecurse = true);
	TXmlNode* __fastcall NodeByElementType(TXmlElementType ElementType);
	virtual TXmlNode* __fastcall NodeByName(const AnsiString AName);
	virtual void __fastcall NodeDelete(int Index);
	void __fastcall NodeExchange(int Index1, int Index2);
	virtual TXmlNode* __fastcall NodeExtract(TXmlNode* ANode);
	virtual TXmlNode* __fastcall NodeFindOrCreate(const AnsiString AName);
	virtual int __fastcall NodeIndexByName(const AnsiString AName);
	virtual int __fastcall NodeIndexByNameFrom(const AnsiString AName, int AFrom);
	int __fastcall NodeIndexOf(TXmlNode* ANode);
	virtual void __fastcall NodeInsert(int Index, TXmlNode* ANode);
	virtual TXmlNode* __fastcall NodeNew(const AnsiString AName);
	virtual TXmlNode* __fastcall NodeNewAtIndex(int Index, const AnsiString AName);
	int __fastcall NodeRemove(TXmlNode* ANode);
	virtual void __fastcall NodesClear(void);
	void __fastcall NodesByName(const AnsiString AName, const Classes::TList* AList);
	virtual bool __fastcall ReadAttributeBool(const AnsiString AName, bool ADefault = false);
	virtual System::TDateTime __fastcall ReadAttributeDateTime(const AnsiString AName, System::TDateTime ADefault = 0.000000E+00);
	virtual int __fastcall ReadAttributeInteger(const AnsiString AName, int ADefault = 0x0);
	virtual __int64 __fastcall ReadAttributeInt64(const AnsiString AName, __int64 ADefault = 0x000000000);
	double __fastcall ReadAttributeFloat(const AnsiString AName, double ADefault = 0.000000E+00);
	virtual AnsiString __fastcall ReadAttributeString(const AnsiString AName, const AnsiString ADefault = "");
	virtual bool __fastcall ReadBool(const AnsiString AName, bool ADefault = false);
	virtual void __fastcall ReadBrush(const AnsiString AName, Graphics::TBrush* ABrush);
	virtual Graphics::TColor __fastcall ReadColor(const AnsiString AName, Graphics::TColor ADefault = (Graphics::TColor)(0x0));
	virtual void __fastcall ReadFont(const AnsiString AName, Graphics::TFont* AFont);
	virtual void __fastcall ReadPen(const AnsiString AName, Graphics::TPen* APen);
	virtual System::TDateTime __fastcall ReadDateTime(const AnsiString AName, System::TDateTime ADefault = 0.000000E+00);
	virtual double __fastcall ReadFloat(const AnsiString AName, double ADefault = 0.000000E+00);
	virtual __int64 __fastcall ReadInt64(const AnsiString AName, __int64 ADefault = 0x000000000);
	virtual int __fastcall ReadInteger(const AnsiString AName, int ADefault = 0x0);
	virtual AnsiString __fastcall ReadString(const AnsiString AName, const AnsiString ADefault = "");
	virtual WideString __fastcall ReadWidestring(const AnsiString AName, const WideString ADefault = L"");
	void __fastcall SortChildNodes(TXMLNodeCompareFunction Compare = 0x0, void * Info = (void *)(0x0));
	AnsiString __fastcall ToAnsiString(const AnsiString s);
	WideString __fastcall ToWidestring(const AnsiString s);
	virtual bool __fastcall ValueAsBoolDef(bool ADefault);
	virtual System::TDateTime __fastcall ValueAsDateTimeDef(System::TDateTime ADefault);
	virtual double __fastcall ValueAsFloatDef(double ADefault);
	virtual __int64 __fastcall ValueAsInt64Def(__int64 ADefault);
	virtual int __fastcall ValueAsIntegerDef(int ADefault);
	virtual void __fastcall WriteAttributeBool(const AnsiString AName, bool AValue, bool ADefault = false);
	virtual void __fastcall WriteAttributeDateTime(const AnsiString AName, System::TDateTime AValue, System::TDateTime ADefault = 0.000000E+00);
	virtual void __fastcall WriteAttributeInteger(const AnsiString AName, int AValue, int ADefault = 0x0);
	virtual void __fastcall WriteAttributeInt64(const AnsiString AName, const __int64 AValue, __int64 ADefault = 0x000000000);
	virtual void __fastcall WriteAttributeFloat(const AnsiString AName, double AValue, double ADefault = 0.000000E+00);
	virtual void __fastcall WriteAttributeString(const AnsiString AName, const AnsiString AValue, const AnsiString ADefault = "");
	virtual void __fastcall WriteBool(const AnsiString AName, bool AValue, bool ADefault = false);
	virtual void __fastcall WriteBrush(const AnsiString AName, Graphics::TBrush* ABrush);
	virtual void __fastcall WriteColor(const AnsiString AName, Graphics::TColor AValue, Graphics::TColor ADefault = (Graphics::TColor)(0x0));
	virtual void __fastcall WriteFont(const AnsiString AName, Graphics::TFont* AFont);
	virtual void __fastcall WritePen(const AnsiString AName, Graphics::TPen* APen);
	virtual void __fastcall WriteDateTime(const AnsiString AName, System::TDateTime AValue, System::TDateTime ADefault = 0.000000E+00);
	virtual void __fastcall WriteFloat(const AnsiString AName, double AValue, double ADefault = 0.000000E+00);
	virtual void __fastcall WriteHex(const AnsiString AName, int AValue, int Digits, int ADefault = 0x0);
	virtual void __fastcall WriteInt64(const AnsiString AName, __int64 AValue, __int64 ADefault = 0x000000000);
	virtual void __fastcall WriteInteger(const AnsiString AName, int AValue, int ADefault = 0x0);
	virtual void __fastcall WriteString(const AnsiString AName, const AnsiString AValue, const AnsiString ADefault = "");
	virtual AnsiString __fastcall WriteToString();
	virtual void __fastcall WriteWidestring(const AnsiString AName, const WideString AValue, const WideString ADefault = L"");
	__property AnsiString AttributeByName[AnsiString AName] = {read=GetAttributeByName, write=SetAttributeByName};
	__property WideString AttributeByNameWide[AnsiString AName] = {read=GetAttributeByNameWide, write=SetAttributeByNameWide};
	__property int AttributeCount = {read=GetAttributeCount, nodefault};
	__property AnsiString AttributeName[int Index] = {read=GetAttributeName, write=SetAttributeName};
	__property AnsiString AttributePair[int Index] = {read=GetAttributePair};
	__property AnsiString AttributeValue[int Index] = {read=GetAttributeValue, write=SetAttributeValue};
	__property WideString AttributeValueAsWidestring[int Index] = {read=GetAttributeValueAsWidestring, write=SetAttributeValueAsWidestring};
	__property int AttributeValueAsInteger[int Index] = {read=GetAttributeValueAsInteger, write=SetAttributeValueAsInteger};
	__property AnsiString AttributeValueDirect[int Index] = {read=GetAttributeValueDirect, write=SetAttributeValueDirect};
	__property TBinaryEncodingType BinaryEncoding = {read=GetBinaryEncoding, write=SetBinaryEncoding, nodefault};
	__property AnsiString BinaryString = {read=GetBinaryString, write=SetBinaryString};
	__property AnsiString CascadedName = {read=GetCascadedName};
	__property TNativeXml* Document = {read=FDocument, write=FDocument};
	__property TXmlElementType ElementType = {read=FElementType, write=FElementType, nodefault};
	__property AnsiString FullPath = {read=GetFullPath};
	__property AnsiString Name = {read=FName, write=SetName};
	__property TXmlNode* Parent = {read=FParent, write=FParent};
	__property int NodeCount = {read=GetNodeCount, nodefault};
	__property TXmlNode* Nodes[int Index] = {read=GetNodes/*, default*/};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property int TotalNodeCount = {read=GetTotalNodeCount, nodefault};
	__property int TreeDepth = {read=GetTreeDepth, nodefault};
	__property bool ValueAsBool = {read=GetValueAsBool, write=SetValueAsBool, nodefault};
	__property System::TDateTime ValueAsDateTime = {read=GetValueAsDateTime, write=SetValueAsDateTime};
	__property __int64 ValueAsInt64 = {read=GetValueAsInt64, write=SetValueAsInt64};
	__property int ValueAsInteger = {read=GetValueAsInteger, write=SetValueAsInteger, nodefault};
	__property double ValueAsFloat = {read=GetValueAsFloat, write=SetValueAsFloat};
	__property AnsiString ValueAsString = {read=GetValueAsString, write=SetValueAsString};
	__property WideString ValueAsWidestring = {read=GetValueAsWidestring, write=SetValueAsWidestring};
	__property AnsiString ValueDirect = {read=FValue, write=FValue};
	__property bool WriteOnDefault = {read=GetWriteOnDefault, nodefault};
};


class DELPHICLASS TXmlNodeList;
class PASCALIMPLEMENTATION TXmlNodeList : public Classes::TList 
{
	typedef Classes::TList inherited;
	
public:
	TXmlNode* operator[](int Index) { return Items[Index]; }
	
private:
	TXmlNode* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, const TXmlNode* Value);
	
public:
	TXmlNode* __fastcall ByAttribute(const AnsiString AName, const AnsiString AValue);
	__property TXmlNode* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	#pragma option push -w-inl
	/* TList.Destroy */ inline __fastcall virtual ~TXmlNodeList(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TXmlNodeList(void) : Classes::TList() { }
	#pragma option pop
	
};


typedef Byte TBigByteArray[2147483647];

typedef TBigByteArray *PBigByteArray;

class DELPHICLASS TsdBufferedReadStream;
class PASCALIMPLEMENTATION TsdBufferedReadStream : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	Classes::TStream* FStream;
	TBigByteArray *FBuffer;
	int FPage;
	int FBufPos;
	int FBufSize;
	int FPosition;
	bool FOwned;
	bool FMustCheck;
	
protected:
	void __fastcall CheckPosition(void);
	
public:
	__fastcall TsdBufferedReadStream(Classes::TStream* AStream, bool Owned);
	__fastcall virtual ~TsdBufferedReadStream(void);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Write(const void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


class DELPHICLASS TsdBufferedWriteStream;
class PASCALIMPLEMENTATION TsdBufferedWriteStream : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	Classes::TStream* FStream;
	TBigByteArray *FBuffer;
	int FBufPos;
	int FPosition;
	bool FOwned;
	
protected:
	void __fastcall Flush(void);
	
public:
	__fastcall TsdBufferedWriteStream(Classes::TStream* AStream, bool Owned);
	__fastcall virtual ~TsdBufferedWriteStream(void);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Write(const void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


class DELPHICLASS TsdAnsiStream;
class PASCALIMPLEMENTATION TsdAnsiStream : public TsdCodecStream 
{
	typedef TsdCodecStream inherited;
	
protected:
	virtual Byte __fastcall ReadByte(void);
	virtual void __fastcall WriteByte(const Byte B);
	virtual void __fastcall WriteBuf(const void *Buffer, int Offset, int Count);
public:
	#pragma option push -w-inl
	/* TsdCodecStream.Create */ inline __fastcall virtual TsdAnsiStream(Classes::TStream* AStream) : TsdCodecStream(AStream) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TsdAnsiStream(void) { }
	#pragma option pop
	
};


class DELPHICLASS TsdUtf8Stream;
class PASCALIMPLEMENTATION TsdUtf8Stream : public TsdCodecStream 
{
	typedef TsdCodecStream inherited;
	
protected:
	virtual Byte __fastcall ReadByte(void);
	virtual void __fastcall WriteByte(const Byte B);
	virtual void __fastcall WriteBuf(const void *Buffer, int Offset, int Count);
public:
	#pragma option push -w-inl
	/* TsdCodecStream.Create */ inline __fastcall virtual TsdUtf8Stream(Classes::TStream* AStream) : TsdCodecStream(AStream) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TsdUtf8Stream(void) { }
	#pragma option pop
	
};


class DELPHICLASS TsdSurplusReader;
class PASCALIMPLEMENTATION TsdSurplusReader : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TStream* FStream;
	AnsiString FSurplus;
	
public:
	__fastcall TsdSurplusReader(Classes::TStream* AStream);
	__property AnsiString Surplus = {read=FSurplus, write=FSurplus};
	int __fastcall ReadChar(char &Ch);
	bool __fastcall ReadCharSkipBlanks(char &Ch);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TsdSurplusReader(void) { }
	#pragma option pop
	
};


class DELPHICLASS TsdStringBuilder;
class PASCALIMPLEMENTATION TsdStringBuilder : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	char operator[](int Index) { return Data[Index]; }
	
private:
	AnsiString FData;
	int FCurrentIdx;
	char __fastcall GetData(int Index);
	void __fastcall Reallocate(int RequiredLength);
	
public:
	__fastcall TsdStringBuilder(void);
	void __fastcall Clear(void);
	void __fastcall AddChar(char Ch);
	void __fastcall AddString(AnsiString &S);
	AnsiString __fastcall StringCopy(int AFirst, int ALength);
	AnsiString __fastcall Value();
	__property int Length = {read=FCurrentIdx, nodefault};
	__property char Data[int Index] = {read=GetData/*, default*/};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TsdStringBuilder(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define cNativeXmlVersion "2.38"
extern PACKAGE TXmlCompareOptions xcAll;
extern PACKAGE AnsiString cDefaultEncodingString;
extern PACKAGE TStringEncodingType cDefaultExternalEncoding;
extern PACKAGE AnsiString cDefaultVersionString;
extern PACKAGE TXmlFormatType cDefaultXmlFormat;
extern PACKAGE bool cDefaultWriteOnDefault;
extern PACKAGE TBinaryEncodingType cDefaultBinaryEncoding;
extern PACKAGE bool cDefaultUtf8Encoded;
extern PACKAGE AnsiString cDefaultIndentString;
extern PACKAGE bool cDefaultDropCommentsOnParse;
extern PACKAGE bool cDefaultUseFullNodes;
extern PACKAGE bool cDefaultSortAttributes;
extern PACKAGE bool cDefaultFloatAllowScientific;
extern PACKAGE int cDefaultFloatSignificantDigits;
extern PACKAGE System::ResourceString _sxeErrorCalcStreamLength;
#define Nativexml_sxeErrorCalcStreamLength System::LoadResourceString(&Nativexml::_sxeErrorCalcStreamLength)
extern PACKAGE System::ResourceString _sxeMissingDataInBinaryStream;
#define Nativexml_sxeMissingDataInBinaryStream System::LoadResourceString(&Nativexml::_sxeMissingDataInBinaryStream)
extern PACKAGE System::ResourceString _sxeMissingElementName;
#define Nativexml_sxeMissingElementName System::LoadResourceString(&Nativexml::_sxeMissingElementName)
extern PACKAGE System::ResourceString _sxeMissingCloseTag;
#define Nativexml_sxeMissingCloseTag System::LoadResourceString(&Nativexml::_sxeMissingCloseTag)
extern PACKAGE System::ResourceString _sxeMissingDataAfterGreaterThan;
#define Nativexml_sxeMissingDataAfterGreaterThan System::LoadResourceString(&Nativexml::_sxeMissingDataAfterGreaterThan)
extern PACKAGE System::ResourceString _sxeMissingLessThanInCloseTag;
#define Nativexml_sxeMissingLessThanInCloseTag System::LoadResourceString(&Nativexml::_sxeMissingLessThanInCloseTag)
extern PACKAGE System::ResourceString _sxeIncorrectCloseTag;
#define Nativexml_sxeIncorrectCloseTag System::LoadResourceString(&Nativexml::_sxeIncorrectCloseTag)
extern PACKAGE System::ResourceString _sxeIllegalCharInNodeName;
#define Nativexml_sxeIllegalCharInNodeName System::LoadResourceString(&Nativexml::_sxeIllegalCharInNodeName)
extern PACKAGE System::ResourceString _sxeMoreThanOneRootElement;
#define Nativexml_sxeMoreThanOneRootElement System::LoadResourceString(&Nativexml::_sxeMoreThanOneRootElement)
extern PACKAGE System::ResourceString _sxeMoreThanOneDeclaration;
#define Nativexml_sxeMoreThanOneDeclaration System::LoadResourceString(&Nativexml::_sxeMoreThanOneDeclaration)
extern PACKAGE System::ResourceString _sxeDeclarationMustBeFirstElem;
#define Nativexml_sxeDeclarationMustBeFirstElem System::LoadResourceString(&Nativexml::_sxeDeclarationMustBeFirstElem)
extern PACKAGE System::ResourceString _sxeMoreThanOneDoctype;
#define Nativexml_sxeMoreThanOneDoctype System::LoadResourceString(&Nativexml::_sxeMoreThanOneDoctype)
extern PACKAGE System::ResourceString _sxeDoctypeAfterRootElement;
#define Nativexml_sxeDoctypeAfterRootElement System::LoadResourceString(&Nativexml::_sxeDoctypeAfterRootElement)
extern PACKAGE System::ResourceString _sxeNoRootElement;
#define Nativexml_sxeNoRootElement System::LoadResourceString(&Nativexml::_sxeNoRootElement)
extern PACKAGE System::ResourceString _sxeIllegalElementType;
#define Nativexml_sxeIllegalElementType System::LoadResourceString(&Nativexml::_sxeIllegalElementType)
extern PACKAGE System::ResourceString _sxeCDATAInRoot;
#define Nativexml_sxeCDATAInRoot System::LoadResourceString(&Nativexml::_sxeCDATAInRoot)
extern PACKAGE System::ResourceString _sxeRootElementNotDefined;
#define Nativexml_sxeRootElementNotDefined System::LoadResourceString(&Nativexml::_sxeRootElementNotDefined)
extern PACKAGE System::ResourceString _sxeCodecStreamNotAssigned;
#define Nativexml_sxeCodecStreamNotAssigned System::LoadResourceString(&Nativexml::_sxeCodecStreamNotAssigned)
extern PACKAGE System::ResourceString _sxeUnsupportedEncoding;
#define Nativexml_sxeUnsupportedEncoding System::LoadResourceString(&Nativexml::_sxeUnsupportedEncoding)
extern PACKAGE System::ResourceString _sxeCannotReadCodecForWriting;
#define Nativexml_sxeCannotReadCodecForWriting System::LoadResourceString(&Nativexml::_sxeCannotReadCodecForWriting)
extern PACKAGE System::ResourceString _sxeCannotWriteCodecForReading;
#define Nativexml_sxeCannotWriteCodecForReading System::LoadResourceString(&Nativexml::_sxeCannotWriteCodecForReading)
extern PACKAGE System::ResourceString _sxeCannotReadMultipeChar;
#define Nativexml_sxeCannotReadMultipeChar System::LoadResourceString(&Nativexml::_sxeCannotReadMultipeChar)
extern PACKAGE System::ResourceString _sxeCannotPerformSeek;
#define Nativexml_sxeCannotPerformSeek System::LoadResourceString(&Nativexml::_sxeCannotPerformSeek)
extern PACKAGE System::ResourceString _sxeCannotSeekBeforeReadWrite;
#define Nativexml_sxeCannotSeekBeforeReadWrite System::LoadResourceString(&Nativexml::_sxeCannotSeekBeforeReadWrite)
extern PACKAGE System::ResourceString _sxeCannotSeek;
#define Nativexml_sxeCannotSeek System::LoadResourceString(&Nativexml::_sxeCannotSeek)
extern PACKAGE System::ResourceString _sxeCannotWriteToOutputStream;
#define Nativexml_sxeCannotWriteToOutputStream System::LoadResourceString(&Nativexml::_sxeCannotWriteToOutputStream)
extern PACKAGE System::ResourceString _sxeXmlNodeNotAssigned;
#define Nativexml_sxeXmlNodeNotAssigned System::LoadResourceString(&Nativexml::_sxeXmlNodeNotAssigned)
extern PACKAGE System::ResourceString _sxeCannotConverToBool;
#define Nativexml_sxeCannotConverToBool System::LoadResourceString(&Nativexml::_sxeCannotConverToBool)
extern PACKAGE System::ResourceString _sxeCannotConvertToFloat;
#define Nativexml_sxeCannotConvertToFloat System::LoadResourceString(&Nativexml::_sxeCannotConvertToFloat)
extern PACKAGE System::ResourceString _sxeSignificantDigitsOutOfRange;
#define Nativexml_sxeSignificantDigitsOutOfRange System::LoadResourceString(&Nativexml::_sxeSignificantDigitsOutOfRange)
extern PACKAGE AnsiString __fastcall EscapeString(const AnsiString AValue);
extern PACKAGE AnsiString __fastcall UnEscapeStringUTF8(const AnsiString AValue);
extern PACKAGE AnsiString __fastcall UnEscapeStringANSI(const AnsiString AValue);
extern PACKAGE AnsiString __fastcall QuoteString(const AnsiString AValue);
extern PACKAGE AnsiString __fastcall UnQuoteString(const AnsiString AValue);
extern PACKAGE AnsiString __fastcall AddControlChars(const AnsiString AValue, const AnsiString Chars, int Interval);
extern PACKAGE AnsiString __fastcall RemoveControlChars(const AnsiString AValue);
extern PACKAGE bool __fastcall FindString(const AnsiString SubString, const AnsiString S, int Start, int Close, int &APos);
extern PACKAGE bool __fastcall MatchString(const AnsiString SubString, const AnsiString S, int Start);
extern PACKAGE void __fastcall ParseAttributes(const AnsiString AValue, int Start, int Close, Classes::TStrings* Attributes);
extern PACKAGE bool __fastcall TrimPos(const AnsiString AValue, int &Start, int &Close);
extern PACKAGE System::TDateTime __fastcall sdDateTimeFromString(const AnsiString ADate);
extern PACKAGE System::TDateTime __fastcall sdDateTimeFromStringDefault(const AnsiString ADate, System::TDateTime ADefault);
extern PACKAGE AnsiString __fastcall sdDateTimeToString(System::TDateTime ADate);
extern PACKAGE AnsiString __fastcall sdWriteNumber(double Value, int SignificantDigits, bool AllowScientific);
extern PACKAGE AnsiString __fastcall sdUnicodeToUtf8(const WideString W);
extern PACKAGE WideString __fastcall sdUtf8ToUnicode(const AnsiString S);
extern PACKAGE AnsiString __fastcall EncodeBase64(const AnsiString Source);
extern PACKAGE AnsiString __fastcall DecodeBase64(const AnsiString Source);
extern PACKAGE AnsiString __fastcall sdAnsiToUtf8(const AnsiString S);
extern PACKAGE AnsiString __fastcall sdUtf8ToAnsi(const AnsiString S);
extern PACKAGE AnsiString __fastcall EncodeBinHex(const AnsiString Source);
extern PACKAGE AnsiString __fastcall DecodeBinHex(const AnsiString Source);

}	/* namespace Nativexml */
using namespace Nativexml;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Nativexml
