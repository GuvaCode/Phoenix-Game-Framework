unit uFont.Kernings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,

  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx;

type
  TFrmFontKernings = class(TFrame)
    lwKernings: TListView;
  private
    FFont: TPHXFont;
    procedure lwKerningsUpdate;

    procedure SetFont(const Value: TPHXFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnableControls(const Enabled: Boolean);

    Property Font: TPHXFont read FFont write SetFont;
  end;

implementation

{$R *.dfm}

{ TFrmKernings }

constructor TFrmFontKernings.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TFrmFontKernings.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmFontKernings.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin

//  edName   .Color:= EnabledColors[Enabled];
//  edLineHeight       .Enabled:= Enabled;

  lwKernings   .Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmFontKernings.lwKerningsUpdate;
var Index  : Integer;
var Kerning: TPHXKerning;
var Item   : TListItem;
var IndexSel: Integer;
begin
//  IndexTop:= lwKernings.TopIndex;
  IndexSel:= lwKernings.ItemIndex;

  lwKernings.Items.BeginUpdate;
  lwKernings.Items.Clear;
  for Index := 0 to Font.Kernings.Count - 1 do
  begin
    Kerning:= Font.Kernings[Index];

    Item:= lwKernings.Items.Add;
    Item.Caption:= IntToStr(Kerning.First);
    Item.SubItems.Add( IntToStr(Kerning.Second)  );
    Item.SubItems.Add( IntToStr(Kerning.Amount)  );
  end;

  lwKernings.Items.EndUpdate;

  if IndexSel < lwKernings.Items.Count then
  begin
    lwKernings.ItemIndex:= IndexSel;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontKernings.SetFont(const Value: TPHXFont);
begin
  FFont := Value;

  if Assigned(FFont) then
  begin
    EnableControls(True);

    lwKerningsUpdate;
  end else
  begin
    EnableControls(False);

  end;
end;



end.
