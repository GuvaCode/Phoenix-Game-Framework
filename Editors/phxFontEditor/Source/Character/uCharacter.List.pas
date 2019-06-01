unit uCharacter.List;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ToolWin, ImgList,

  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx,

  uCharacter.Detail;

type
  TFrmFontCharacters = class(TFrame)
    pnRight: TPanel;
    Panel1: TPanel;
    Panel6: TPanel;
    HeaderControl1: THeaderControl;
    lwCharacters: TListBox;
    Splitter1: TSplitter;
    ListImages: TImageList;
    ToolBar1: TToolBar;
    btnSearch: TToolButton;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    ToolButton2: TToolButton;
    procedure lwCharactersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lwCharactersClick(Sender: TObject);
    procedure CharacterChanged(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FFont      : TPHXFont;
    FBuffer    : TBitmap;
    FBackground: TBitmap;


    FrmCharacter: TFrmCharacter;
    FOnChange: TNotifyEvent;
    procedure UpdateCharacterList;

    procedure SetFont(const Value: TPHXFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnableControls(const Enabled: Boolean);

    Property Font: TPHXFont read FFont write SetFont;

    property Buffer: TBitmap read FBuffer;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses uCharacter.Search;


// TFrmFont
//------------------------------------------------------------------------------
constructor TFrmFontCharacters.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);

  FrmCharacter:= TFrmCharacter.Create(Self);
  FrmCharacter.Parent:= pnRight;
  FrmCharacter.Align:= alClient;
  FrmCharacter.OnChange:= CharacterChanged;
  FrmCharacter.EnableControls(False);
end;

//------------------------------------------------------------------------------
destructor TFrmFontCharacters.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  btnSearch   .Enabled:= Enabled;
  btnAdd      .Enabled:= Enabled;
  btnDelete   .Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.SetFont(const Value: TPHXFont);
var Index: Integer;
begin
  FFont := Value;

  if Assigned(FFont) then
  begin
    EnableControls(True);

    DrawTexture(Font.Texture, FBuffer, FBackground);

    HeaderControl1.Sections[0].Width:=0;

    for Index := 0 to Font.Characters.Count - 1 do
    begin
      if Font.Characters[Index].Width + 4 > HeaderControl1.Sections[0].Width then
      begin
        HeaderControl1.Sections[0].Width:= Font.Characters[Index].Width + 4;
      end;
    end;
  end else
  begin
    EnableControls(False);
    FrmCharacter.EnableControls(False);

    FBuffer.Width := 0;
    FBuffer.Height:= 0;

  end;
  FrmCharacter.Font:= FFont;

  UpdateCharacterList;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.CharacterChanged(Sender: TObject);
var Index: Integer;
begin
  Index:= lwCharacters.ItemIndex;

  if Assigned(Font) and (Index >= 0) and (Index < Font.Characters.Count) then
  begin
    Font.Characters[Index]:= FrmCharacter.Character;

    if Assigned(OnChange) then OnChange(Self);

    lwCharacters.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmFontCharacters.UpdateCharacterList;
var Index: Integer;
var Character: PPHXCharacter;
var IndexTop: Integer;
var IndexSel: Integer;
begin
  IndexTop:= lwCharacters.TopIndex;
  IndexSel:= lwCharacters.ItemIndex;

  if Assigned(Font) then
  begin

    if lwCharacters.Items.Count <> Font.Characters.Count then
    begin
     lwCharacters.Items.BeginUpdate;

      if Font.Size >= 12 then
      begin
        lwCharacters.ItemHeight:= Font.Size + 4;
      end else
      begin
        lwCharacters.ItemHeight:= 16;
      end;

      lwCharacters.Items.Clear;

      for Index := 0 to Font.Characters.Count - 1 do
      begin
        Character:= @Font.Characters.List^[Index];

        lwCharacters.Items.Add( IntToStr(Character^.ID) );

        if Character^.Height + 4 > lwCharacters.ItemHeight then
        begin
          lwCharacters.ItemHeight:= Character^.Height + 4;
        end;
      end;
      lwCharacters.Items.EndUpdate;
  end;

  end else
  begin
    lwCharacters.Items.Clear;
  end;


  {

  lwCharacters.Items.BeginUpdate;
  lwCharacters.Items.Clear;

  if Assigned(Font) then
  begin
//    lwCharacters.ItemHeight:= Font.LineHeight + 4;
    if Font.Size >= 12 then
    begin
      lwCharacters.ItemHeight:= Font.Size + 4;
    end else
    begin
      lwCharacters.ItemHeight:= 16;
    end;


    for Index := 0 to Font.Characters.Count - 1 do
    begin
      Character:= @Font.Characters.List^[Index];

      lwCharacters.Items.Add( IntToStr(Character^.ID) );

      if Character^.Height + 4 > lwCharacters.ItemHeight then lwCharacters.ItemHeight:= Character^.Height + 4;
    end;
  end;
  lwCharacters.Items.EndUpdate;
       }
  if IndexTop < lwCharacters.Items.Count then
  begin
    lwCharacters.TopIndex:= IndexTop;
  end;
  if IndexSel < lwCharacters.Items.Count then
  begin
    lwCharacters.ItemIndex:= IndexSel;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.lwCharactersClick(Sender: TObject);
var Index: Integer;
begin
  Index:= lwCharacters.ItemIndex;

  if Assigned(Font) and (Index >= 0) and (Index < Font.Characters.Count) then
  begin
    FrmCharacter.Character:= Font.Characters[Index];

    FrmCharacter.EnableControls(True);
  end else
  begin
    FrmCharacter.EnableControls(False);
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmFontCharacters.lwCharactersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Character: TPHXCharacter;
var DstRect: TRect;
var SrcRect: TRect;
var X,Y: Integer;
begin
  if Font = nil then
  begin
    Exit;
  end;
  
  Character:= Font.Characters[Index];

  with lwCharacters.Canvas do
  begin
    FillRect(Rect);

    DstRect.Left  := Rect.Left + 2;
    DstRect.Top   := Rect.Top  + 2;
    DstRect.Right := DstRect.Left + Character.Width;
    DstRect.Bottom:= DstRect.Top  + Character.Height;
 
    SrcRect.Left  := Character.X;
    SrcRect.Top   := Character.Y;
    SrcRect.Right := Character.X + Character.Width;
    SrcRect.Bottom:= Character.Y + Character.Height;
 //    Character.X,
//    Character.Y

    CopyRect(DstRect, FBuffer.Canvas, SrcRect);

    X:= HeaderControl1.Sections[0].Width;
    Y:= Rect.Top + (lwCharacters.ItemHeight + Font.Height) div 2;

    TextOut(X,Y , Format('%.3d', [Character.ID]));

    Inc(X, HeaderControl1.Sections[1].Width);

    TextOut(X, Y, Chr(Character.ID));

  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.btnAddClick(Sender: TObject);
var Character: TPHXCharacter;
begin
  Character.ID       := 0;
  Character.X        := 0;
  Character.Y        := 0;
  Character.Width    := Font.Metric.Height;
  Character.Height   := Font.Metric.Height;
  Character.Offset.X := 0;
  Character.Offset.Y := 0;
  Character.Advance  :=  Character.Width;

  Font.Characters.Add(Character);

  UpdateCharacterList;

  lwCharacters.ItemIndex:= Font.Characters.Count-1;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.btnDeleteClick(Sender: TObject);
var Index: Integer;
begin
  Index:= lwCharacters.ItemIndex;

  if Assigned(Font) and (Index >= 0) and (Index < Font.Characters.Count) then
  begin
    Font.Characters.Delete(Index);

    UpdateCharacterList;

    FrmCharacter.EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontCharacters.btnSearchClick(Sender: TObject);
var Index: Integer;
var Char : WideChar;
var Number: Cardinal;
begin
  if FrmCharacterSearch.ShowModal = mrOk then
  begin
    Index:= -1;
    if FrmCharacterSearch.edChar.Checked then
    begin
      Char:= FrmCharacterSearch.edSearch.Text[1];

      Index:= Font.Characters.Find(Char);
    end;
    if FrmCharacterSearch.edNumber.Checked then
    begin
      Number:= StrToIntDef(FrmCharacterSearch.edSearch.Text, 0);

      Index:= Font.Characters.Find(Number);
    end;

    if (Index >= 0) and (Index < Font.Characters.Count) then
    begin
      FrmCharacter.Character:= Font.Characters[Index];

      FrmCharacter.EnableControls(True);
    end else
    begin
      FrmCharacter.EnableControls(False);
    end;

    lwCharacters.ItemIndex:= Index;
  end;
  (*
  if InputQuery('Search for character', 'Character:', Text) then
  begin
    if Length(Text) = 0 then Exit;

    Index:= Font.Characters.Find(Text[1]);

    if (Index >= 0) and (Index < Font.Characters.Count) then
    begin
      FrmCharacter.Character:= Font.Characters[Index];

      FrmCharacter.EnableControls(True);
    end else
    begin
      FrmCharacter.EnableControls(False);
    end;

    lwCharacters.ItemIndex:= Index;
  end;
  *)
end;


end.
