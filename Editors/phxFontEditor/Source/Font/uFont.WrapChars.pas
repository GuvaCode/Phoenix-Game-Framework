unit uFont.WrapChars;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

{$IFnDEF FPC}
  Vcl.StdCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.Controls, Vcl.Graphics, System.Classes, System.Variants, System.SysUtils, Winapi.Messages, Winapi.Windows, Vcl.Grids,
{$ELSE}
{$ENDIF}
  phxFont;

type
  TFrmWrapCharacters = class(TForm)
    sgCharacters: TStringGrid;
    btnOk: TButton;
    btnCancel: TButton;
    procedure sgCharactersSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure sgCharactersSelectCell(Sender: TObject; ACol, ARow: Integer;   var CanSelect: Boolean);
    procedure sgCharactersExit(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure sgCharactersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCharacters: WideString;

    SelRow: Integer;
    SelCol: Integer;
    SelValue: String;

    procedure SaveValue;

    function GetCharacters: WideString;
    procedure SetCharacters(const Value: WideString);
  public
    constructor Create(AOwner: TComponent); override;

    function Execute(const Caption: String): Boolean;

    property Characters: WideString read GetCharacters write SetCharacters;
  end;

var
  FrmWrapCharacters: TFrmWrapCharacters;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

// TFrmWrapCharacters
//------------------------------------------------------------------------------
constructor TFrmWrapCharacters.Create(AOwner: TComponent);
begin
  inherited;

  SelRow := -1;
  SelCol := -1;
  SelValue:= '';

  sgCharacters.Cells[0, 0]:= 'Ident';
  sgCharacters.Cells[1, 0]:= 'Character';
end;

//------------------------------------------------------------------------------
function TFrmWrapCharacters.Execute(const Caption: String): Boolean;
begin
  Self.Caption:= Caption;

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.SaveValue;
var Char : WideChar;
begin
  if Length(SelValue) < 1 then Exit;

  case SelCol of
    0: Char:= Chr(StrToIntDef(SelValue, 0));
    1: Char:= SelValue[1];
  end;

  if Char <> #0 then
  begin
    sgCharacters.Cells[0, SelRow]:= IntToStr( Ord(Char )) ;
    sgCharacters.Cells[1, SelRow]:= Char;
  end else
  begin
    sgCharacters.Cells[0, SelRow]:= '';
    sgCharacters.Cells[1, SelRow]:= '';
  end;
end;

//
//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.sgCharactersExit(Sender: TObject);
begin
  if (SelRow <> -1) or (SelCol <> -1) then
  begin
    SaveValue;

    SelRow  := -1;
    SelCol  := -1;
    SelValue:= '';
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.sgCharactersKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    SaveValue;
  end;
  if Key = VK_DELETE then
  begin
    sgCharacters.Cells[0, SelRow]:= '' ;
    sgCharacters.Cells[1, SelRow]:= '';

    SelValue:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.sgCharactersSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (SelRow <> ARow) or (SelCol <> ACol) then
  begin
    SaveValue;

    SelValue:= '';
    SelRow  := ARow;
    SelCol  := ACol;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.sgCharactersSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if (SelRow <> ARow) or (SelCol <> ACol) then
  begin
    SaveValue;
  end;

  if (Value <> '') and (SelRow + 1 = sgCharacters.RowCount) then
  begin
    sgCharacters.RowCount:= sgCharacters.RowCount + 1;
  end;

  SelRow  := ARow;
  SelCol  := ACol;
  SelValue:= Value;
end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.btnOkClick(Sender: TObject);
begin
  if (SelRow <> -1) or (SelCol <> -1) then
  begin
    SaveValue;

    SelValue:= '';
    SelRow := -1;
    SelCol := -1;
  end;
end;

//------------------------------------------------------------------------------
function TFrmWrapCharacters.GetCharacters: WideString;
var Index: Integer;
var Char : WideChar;
begin
  FCharacters:= '';
  for Index := 1 to sgCharacters.RowCount -1 do
  begin
    Char:= Chr( StrToIntDef(sgCharacters.Cells[0, Index], 0));

    if Char <> #0 then
    begin
      FCharacters:= FCharacters + Char;
    end;
  end;

  Result:= FCharacters;
end;

//------------------------------------------------------------------------------
procedure TFrmWrapCharacters.SetCharacters(const Value: WideString);
var Index: Integer;
var Char : WideChar;
begin
  FCharacters:= Value;

  sgCharacters.RowCount:= 1 + Length(FCharacters) + 1;
  for Index := 1 to Length(Value) do
  begin
    Char:= Value[Index];
    sgCharacters.Cells[0, Index]:= IntToStr( Ord(Char )) ;
    sgCharacters.Cells[1, Index]:= Char;
  end;
end;




end.
