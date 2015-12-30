unit sdConnectFour;
{ Game logic for connect-four

}

interface

uses
  Classes, SysUtils, Contnrs, Graphics;

const
  cColCount = 7; // Number of columns in connect-four
  cRowCount = 6; // Number or rows in connect-four

type

  TsdPlayer = class;
  TsdMoves = class;

  TsdField = (
    ftNone,
    ftPlayer1,
    ftPlayer2
  );

  PsdPose = ^TsdPose;
  TsdPose = array[0..cColCount * cRowCount - 1] of TsdField;

  TsdGame = class(TPersistent)
  private
    FPlayer1: TsdPlayer;
    FPlayer2: TsdPlayer;
    FHistory: TsdMoves;
    FCurrentPose: TsdPose;
    function GetPlayers(Index: integer): TsdPlayer;
    function GetCurrentPose: PsdPose;
    function GetCurrentPlayer: TsdPlayer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Players[Index: integer]: TsdPlayer read GetPlayers;
    property Player1: TsdPlayer read FPlayer1;
    property Player2: TsdPlayer read FPlayer2;
    property CurrentPose: PsdPose read GetCurrentPose;
    property CurrentPlayer: TsdPlayer read GetCurrentPlayer;
  end;

  TsdMove = class
  end;

  TsdMoves = class(TObjectList)
  end;

  TsdPlayer = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TsdStrategy = class
  private
    FWinner: TsdField;
  public
    procedure EvaluatePose(const APose: TsdPose);
    property Winner: TsdField read FWinner;
  end;

  PsdBranch = ^TsdBranch;
  TsdBranch = record
    Move: byte;
    Subs: array of PsdBranch;
  end;

  TsdTree = class
  private
    FPose: PsdPose;
    FRoot: TsdBranch;
  public
    destructor Destroy; override;
    procedure CreateBranches(APose: PsdPose; ARoot: PsdBranch; ADepth: integer; APlayer: TsdField);
  end;

implementation

{ TsdGame }

constructor TsdGame.Create;
begin
  FPlayer1 := TsdPlayer.Create;
  FPlayer1.Name := 'Player 1';
  FPlayer2 := TsdPlayer.Create;
  FPlayer2.Name := 'Player 2';
  FHistory := TsdMoves.Create(True);
end;

destructor TsdGame.Destroy;
begin
  FreeAndNil(FPlayer1);
  FreeAndNil(FPlayer2);
  FreeAndNil(FHistory);
  inherited;
end;

function TsdGame.GetCurrentPlayer: TsdPlayer;
begin
  if Odd(FHistory.Count) then
    Result := FPlayer2
  else
    Result := FPlayer1;
end;

function TsdGame.GetCurrentPose: PsdPose;
begin
  Result := @FCurrentPose;
end;

function TsdGame.GetPlayers(Index: integer): TsdPlayer;
begin
  if Index = 0 then
    Result := FPlayer1
  else
    Result := FPlayer2;
end;

{ TsdStrategy }

procedure TsdStrategy.EvaluatePose(const APose: TsdPose);
var
  r, c, f: integer;
  p: TsdField;
begin
  FWinner := ftNone;
  // Check the board for any connect-fours
  for c := 0 to cColCount - 1 do
  begin
    for r := cRowCount - 1 downto 0 do
    begin
      f := r * cColCount + c;
      p := APose[f];
      if p = ftNone then
        // continue to next col
        break;

      if c <= 3 then
      begin
        // Test dia up
        if r >= 3 then
        begin
          if (APose[f - 6] = p) and (APose[f - 12] = p) and (APose[f - 18] = p) then
          begin
            FWinner := p;
            exit;
          end;
        end;
        // test horizontal
        if (APose[f + 1] = p) and (APose[f + 2] = p) and (APose[f + 3] = p) then
        begin
          FWinner := p;
          exit;
        end;
        // test dia down
        if r <= 2 then
        begin
          if (APose[f + 8] = p) and (APose[f + 16] = p) and (APose[f + 24] = p) then
          begin
            FWinner := p;
            exit;
          end;
        end;
      end;
      if r <= 2 then
      begin
        // test vertical
        if (APose[f + 7] = p) and (APose[f + 14] = p) and (APose[f + 21] = p) then
        begin
          FWinner := p;
          exit;
        end;
      end;
    end;
  end;
end;

{ TsdTree }

procedure TsdTree.CreateBranches(APose: PsdPose; ARoot: PsdBranch; ADepth: integer; APlayer: TsdField);
begin
  // Based on APose, we assume it is APlayer's move and we create subbranches from ARoot until ADepth


end;

destructor TsdTree.Destroy;
  procedure FreeBranch(B: PsdBranch);
  var
    i: integer;
  begin
    for i := 0 to length(B.Subs) - 1 do
      FreeBranch(B.Subs[i]);
    Dispose(B);
  end;
var
  i: integer;
begin
  for i := 0 to length(FRoot.Subs) - 1 do
    FreeBranch(FRoot.Subs[i]);
  inherited;
end;

end.
