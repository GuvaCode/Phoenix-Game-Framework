unit uPatternSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  phxImage, StdCtrls;

type
  TFrmPatternSearch = class(TForm)
    edSearch: TEdit;
    lwPatterns: TListBox;
    procedure edSearchChange(Sender: TObject);
    procedure lwPatternsDblClick(Sender: TObject);
    procedure lwPatternsClick(Sender: TObject);
    procedure lwPatternsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FImage       : TPHXImage;
    FSearch      : String;
    FPatternIndex: Integer;


    procedure DoSearch;

    procedure SetImage(const Value: TPHXImage);
    function SetSearch: String;
  public
    function Execute(const Image: TPHXImage): Boolean;

    Property Image: TPHXImage read FImage write SetImage;

    Property Search: String read SetSearch write FSearch;

    Property PatternIndex: Integer read FPatternIndex write FPatternIndex;
  end;

var
  FrmPatternSearch: TFrmPatternSearch;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.DoSearch;
var Index: Integer;
var Name : String;
begin
  lwPatterns.Items.BeginUpdate;
  lwPatterns.Items.Clear;
  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Name:= String(Image.Patterns[Index].Name);

    if (Length(Search) = 0) or (Pos( LowerCase(Search), LowerCase(Name)) > 0) then
    begin
      lwPatterns.Items.AddObject(Name, Pointer(Index));
    end;

  end;

  lwPatterns.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.edSearchChange(Sender: TObject);
begin
  FSearch:= edSearch.Text;

  DoSearch;

//  edSearch.OnChange:= nil;
//  edSearch.Text:= FindMatchString;
//  edSearch.SelStart:= Length(edSearch.Text);
//  edSearch.OnChange:= edSearchChange;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (lwPatterns.Count = 1) then
  begin
    PatternIndex:= Integer(lwPatterns.Items.Objects[0]);
    ModalResult:= mrOk;
  end;
end;

//------------------------------------------------------------------------------
function TFrmPatternSearch.Execute(const Image: TPHXImage): Boolean;
begin
  SetImage(Image);

  PatternIndex:= -1;

  if ShowModal = mrOk then
  begin
    Result:= PatternIndex <> -1;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.lwPatternsClick(Sender: TObject);
begin
  if lwPatterns.ItemIndex >= 0 then
  begin
    PatternIndex:= Integer(lwPatterns.Items.Objects[lwPatterns.ItemIndex]);
  end else
  begin
    PatternIndex:= -1;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.lwPatternsDblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.lwPatternsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lwPatternsClick(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmPatternSearch.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  DoSearch
end;

//------------------------------------------------------------------------------
function TFrmPatternSearch.SetSearch: String;
begin
  Result := FSearch;

  edSearch.Text:= FSearch;
end;

end.
