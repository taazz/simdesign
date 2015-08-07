{ unit sdPdfObjectTrees

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements traversing of object tree variants
  chapter 3.6.2: Page Tree
  chapter 3.8.5: Name Tree    TO DO
  chapter 3.8.6: Number Tree  TO DO

  Author: Nils Haeck M.Sc.

  Changes:
    06Jan2004 - Created
    25Nov2005 - Added TpdfPageTree.PageAdd

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfObjectTrees;

interface

uses
  Classes, sdPdfObjects, sdPdfUtil;

type

  // PDF Page tree. Use Pages[Index] to access the individual pages in
  // the tree, with Index ranging from 0 to PageCount - 1
  TPdfPageTree = class(TPersistent)
  private
    FTree: TPdfDictionary;
    function GetPageCount: integer;
    function GetPages(Index: integer): TPdfDictionary;
  public
    constructor Create(ATree: TPdfDictionary);
    procedure PageAdd(APage: TpdfDictionary);
    property PageCount: integer read GetPageCount;
    property Pages[Index: integer]: TPdfDictionary read GetPages; default;
  end;

resourcestring

  sUnableToTraverse = 'Unable to traverse tree';

implementation

{ TPdfPageTree }

constructor TPdfPageTree.Create(ATree: TPdfDictionary);
begin
  inherited Create;
  FTree := ATree;
end;

function TPdfPageTree.GetPageCount: integer;
begin
  Result := FTree.IntegerByKeyDefault('Count', 0);
end;

function TPdfPageTree.GetPages(Index: integer): TPdfDictionary;
// local
function TraverseTree(FTree: TPdfDictionary; Index: integer): TPdfDictionary;
// Traverse the tree finding the correct page
var
  i: integer;
  AKids: TPdfArray;
  ANode: TPdfDictionary;
  ACount: integer;
begin
  Result := nil;
  if not (FTree.DictionaryType = 'Pages') then
    raise EPdfError.Create(sUnableToTraverse);
  // Kids
  AKids := FTree.ArrayByKey('Kids');
  if not assigned(AKids) then
    raise EPdfError.CreateFmt(sPdfValueNotFound, ['Kids']);
  for i := 0 to AKids.ElementCount - 1 do
  begin
    ANode := AKids.DictionaryByIndex(i);
    // Determine if its a page or page node
    if ANode.DictionaryType = 'Page' then
    begin
      // Single page
      if Index = 0 then
      begin
        Result := ANode;
        exit;
      end;
      // Substract 1
      dec(Index);
      continue;
    end;
    // Page node
    ACount := ANode.IntegerByKey('Count');
    if Index < ACount then begin
      Result := TraverseTree(ANode, Index);
      exit;
    end;
    // Substract
    Dec(Index, ACount);
  end;
end;
// main
begin
  Result := TraverseTree(FTree, Index);
end;

procedure TPdfPageTree.PageAdd(APage: TpdfDictionary);
var
  AKids: TPdfArray;
begin
  AKids := FTree.ArrayByKey('Kids');
  if not assigned(AKids) then
    raise EPdfError.CreateFmt(sPdfValueNotFound, ['Kids']);
  AKids.ElementsAdd(APage);
end;

end.
