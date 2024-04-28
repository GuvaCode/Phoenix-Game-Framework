unit phxImageDialog;
{$mode Delphi}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,StdCtrls, ComCtrls, ExtCtrls,  LCLType, LCLIntf,

  Generics.Collections,

  phxTypes,
  phxImage,
  phxGraphics,
  phxClasses;


type

TLoadImageEvent = procedure(const FileName: String; out Image: TPHXImage) of object;

//------------------------------------------------------------------------------
TPHXImageDialog = class(TComponent)
  private
      //: TList<>;
    FImages   : TList<TPHXImage>;
    FImage  : TPHXImage;
    FPattern: TPHXPatternIndex;

    FOnLoadImage: TLoadImageEvent;
  public
   constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;

    procedure SetImages(const Images: TPHXImageList); overload;
    procedure SetImages(const Images: TList<TPHXImage>); overload;

    // Selected image
    property Image: TPHXImage read FImage write FImage;
    // Selected pattern index
    property Pattern: TPHXPatternIndex read FPattern write FPattern;

    // Selected image
    property OnLoadImage: TLoadImageEvent read FOnLoadImage write FOnLoadImage;
  end;

TViewStyle = (
  vsImage,
  vsPatterns
  );


//------------------------------------------------------------------------------
TFrmIconSelector = class(TForm)
    Panel1: TPanel;
    lblLoadImage: TLabel;
    btnOkey: TButton;
    btnCancel: TButton;
    cbTransparent: TCheckBox;
    OpenDialog: TOpenDialog;
    TabControl1: TTabControl;
    PaintBox1: TPaintBox;
    ScrollBar1: TScrollBar;
    GroupBox1: TGroupBox;
    lwImages: TListBox;
    btnNone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure lblLoadImageClick(Sender: TObject);
    procedure lwImagesClick(Sender: TObject);
    procedure lwImagesDblClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
  private
    { Private declarations }
    FViewStyle     : TViewStyle;
    FImages        : TList<TPHXImage>;

    FImage   : TPHXImage;
    FPattern : TPHXPatternIndex;

    FBuffer      : TBitmap;
    FBackground  : TBitmap;
    FPatternRects: TList<TRect>;

    FOnLoadImage : TLoadImageEvent;

    Procedure UpdateBuffers;

    Procedure lwImagesUpdate;

    Procedure PaintImage  (Canvas: TCanvas);
    procedure PaintPattern(Canvas: TCanvas);

    function GetSelectedName: String;


    procedure SetViewStyle(const Value: TViewStyle);
    procedure SetLoadImage(const Value: TLoadImageEvent);

    procedure SetImages   (const Value: TList<TPHXImage>);
    procedure SetImage    (const Value: TPHXImage);
    procedure SetPattern  (const Value: TPHXPatternIndex);
  public
    // The selected image
    Property Image: TPHXImage read FImage write SetImage;
    // The selected pattern
    property Pattern: TPHXPatternIndex read FPattern write SetPattern;

    // Selected image
    property OnLoadImage: TLoadImageEvent read FOnLoadImage write SetLoadImage;

    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle;

    Property Images: TList<TPHXImage> read FImages write SetImages;
  end;

procedure Register;


implementation

{$R *.dfm}


Uses phxGraphicsEx;

procedure Register;
begin
  RegisterComponents('Phoenix', [TPHXImageDialog]);
end;



//------------------------------------------------------------------------------
procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; X,Y: Integer; Image: TPHXImage; PatternIndex: Integer);
var SrcRect: TRect;
var DstRect: TRect;
begin

  SrcRect.Left  := Image.Patterns[PatternIndex].X;
  SrcRect.Top   := Image.Patterns[PatternIndex].Y;
  SrcRect.Right := Image.Patterns[PatternIndex].X + Image.Patterns[PatternIndex].Width;
  SrcRect.Bottom:= Image.Patterns[PatternIndex].Y + Image.Patterns[PatternIndex].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Image.Patterns[PatternIndex].Width;
  DstRect.Bottom := Y + Image.Patterns[PatternIndex].Height;

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;


//------------------------------------------------------------------------------
procedure TFrmIconSelector.FormCreate(Sender: TObject);
begin
  DoubleBuffered:= True;

  FImage  := nil;
  FPattern:= -1;

  FBuffer:= TBitmap.Create;

  FPatternRects:= TList<TRect>.Create;

  FBackground:= TBitmap.Create;
  FBackground.PixelFormat:= pf32Bit;
  FBackground.Width      := 8;
  FBackground.Height     := 8;

  DrawTransparent(FBackground.Canvas, Rect(0,0,8,8), 4);

  SetViewStyle(vsPatterns);
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.FormDestroy(Sender: TObject);
begin
  FBackground.Free;
  FBuffer.Free;

  FPatternRects.Free;
end;




//------------------------------------------------------------------------------
procedure TFrmIconSelector.TabControl1Change(Sender: TObject);
begin
  case TabControl1.TabIndex of
    0: SetViewStyle(vsPatterns);
    1: SetViewStyle(vsImage);
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmIconSelector.PaintBox1Paint(Sender: TObject);
begin
  with PaintBox1.Canvas do
  begin
    Brush.Color:= clBtnFace;
    Pen  .Color:= clBtnFace;

    FillRect(PaintBox1.ClientRect);
  end;

  if Image = nil then Exit;

  case ViewStyle of
    vsImage   : PaintImage  ( PaintBox1.Canvas );
    vsPatterns: PaintPattern( PaintBox1.Canvas );
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.PaintImage(Canvas: TCanvas);
begin
  PaintBox1.Canvas.Draw(0,0, FBuffer);
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.PaintPattern(Canvas: TCanvas);
var Index      : Integer;
var X          : Integer;
var Y          : Integer;
var Pattern    : TPHXPattern;
var PatternRect: TRect;

var RowHeight     : Integer;
var ColWidth     : Integer;
begin
  FPatternRects.Clear;
  X        := 2;
  Y        := 2;
  RowHeight:= 16;

  Canvas.Font.Style:= [fsBold];
  Canvas.Brush.Style:= bsClear;

  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Pattern:= Image.Patterns[Index];

    // Calculate the width of the column
    if Canvas.TextWidth(Pattern.Name) > Pattern.Width then
    begin
      ColWidth:= Canvas.TextWidth(Pattern.Name);
    end else
    begin
      ColWidth:= Pattern.Width;
    end;

    // The pattern doesnt fit
    if ColWidth > PaintBox1.Width then Exit;

    // Move to the next line
    if (X + ColWidth > PaintBox1.Width) then
    begin
      Y:= Y + RowHeight + 4;
      X:= 2;

      RowHeight:= 16;
    end;
    //  This pattern is higher then the highest so far
    if Pattern.Height + 16 > RowHeight then RowHeight:= Pattern.Height + 16;

    PatternRect.Left  := X;
    PatternRect.Top   := Y;
    PatternRect.Bottom:= Y + RowHeight;
    PatternRect.Right := X + ColWidth;

    DrawPattern(Canvas, FBuffer, X + (ColWidth - Pattern.Width) div 2, Y+14, Image, Index);

    Canvas.TextOut(x, y, Pattern.Name);

    FPatternRects.Add(PatternRect);

    if Index = Self.Pattern then
    begin
      Canvas.Pen.Color:= clRed;

      PatternRect.Left   := PatternRect.Left   - 1;
      PatternRect.Top    := PatternRect.Top    - 1;
      PatternRect.Right  := PatternRect.Right  + 1;
      PatternRect.Bottom := PatternRect.Bottom + 1;

      Canvas.Rectangle(PatternRect);
    end;

    if Canvas.TextWidth(Pattern.Name) > Pattern.Width then
    begin
      X:= X + Canvas.TextWidth(Pattern.Name) + 4;
    end else
    begin
      X:= X + Pattern.Width + 4;
    end;



  end;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index      : Integer;
var PatternRect: TRect;
begin
  FPattern:= -1;
  for Index := 0 to FPatternRects.Count - 1 do
  begin
    PatternRect:= FPatternRects[Index];

    if PtInRect(PatternRect, Point(X,Y)) then
    begin
      SetPattern(Index);
    end;
  end;
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.btnNoneClick(Sender: TObject);
begin
  FImage  := nil;
  FPattern:= -1;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.cbTransparentClick(Sender: TObject);
begin
  UpdateBuffers;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.lblLoadImageClick(Sender: TObject);
var Index: Integer;
var FileName: String;
var Image: TPHXImage;
begin
  if Assigned(OnLoadImage) and OpenDialog.Execute then
  begin
    for Index := 0 to OpenDialog.Files.Count - 1 do
    begin
      FileName:= OpenDialog.Files[Index];

      OnLoadImage(FileName, Image);

      Assert(Assigned(Image));

      Images.Add(Image);
    end;
    SetImages(Images);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.lwImagesUpdate;
var Image: TPHXImage;
begin
  if not Assigned(FImages) then
  begin
    lwImages.Items.BeginUpdate;
    lwImages.Items.Clear;
    lwImages.Items.EndUpdate;

    Exit;
  end;

  lwImages.Items.BeginUpdate;
  lwImages.Items.Clear;

  for Image in Images do
  begin
    lwImages.Items.Add(Image.Name);
  end;
  lwImages.Items.EndUpdate;

  lwImages.ItemIndex:=Images.IndexOf(FImage);
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.lwImagesClick(Sender: TObject);
var Index: Integer;
begin
  Index:= lwImages.ItemIndex;

  if Assigned(FImages) then
  begin

    if (Index >= 0) and (Index < FImages.Count) then
    begin
      SetImage( Images[Index] );
    end else
    begin
     SetImage( nil);
    end;

  end else
  begin
    SetImage(nil);
  end;
end;
//------------------------------------------------------------------------------
procedure TFrmIconSelector.lwImagesDblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.UpdateBuffers;
begin
  if not Assigned(Image) then Exit;

  if cbTransparent.Checked then
  begin
    DrawTexture(Image.Texture, FBuffer, FBackground);
  end else
  begin
    DrawTexture(Image.Texture, FBuffer);
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
function TFrmIconSelector.GetSelectedName: String;
begin
  IF Image <> nil then
    Result:= Image.Name
  else
    Result:='';
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.SetImages(const Value: TList<TPHXImage>);
begin
  FImages:= Value;

  lwImagesUpdate;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.SetLoadImage(const Value: TLoadImageEvent);
begin
  FOnLoadImage := Value;

  lblLoadImage.Enabled:= Assigned(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(FImage) then
  begin
    UpdateBuffers;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmIconSelector.SetPattern(const Value: TPHXPatternIndex);
begin
  FPattern := Value;

  PaintBox1.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmIconSelector.SetViewStyle(const Value: TViewStyle);
begin
  FViewStyle := Value;

  PaintBox1.Invalidate;
end;




// TPHXImageDialog
//------------------------------------------------------------------------------
constructor TPHXImageDialog.Create(AOwner: TComponent);
begin
  inherited;
  FImages:= TList<TPHXImage>.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXImageDialog.Destroy;
begin
  FImages.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TPHXImageDialog.Execute: Boolean;
var Form: TFrmIconSelector;
begin
  Form:=TFrmIconSelector.Create(Owner);
  try
    Form.Image         := FImage;
    Form.Pattern       := FPattern;
    Form.Images        := FImages;
    Form.OnLoadImage   := OnLoadImage;

    if Form.ShowModal = mrOk then
    begin
      Result:= True;

      FImage  := Form.Image;
      FPattern:= Form.Pattern;
    end else
    begin
      Result:= False;
    end;

  finally
    Form.Free;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXImageDialog.SetImages(const Images: TPHXImageList);
var Index: Integer;
begin
  FImages.Clear;

  for Index := 0 to Images.Count - 1 do
  begin
    FImages.Add( Images[Index] );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageDialog.SetImages(const Images: TList<TPHXImage>);
var Index: Integer;
begin
  FImages.Clear;

  for Index := 0 to Images.Count - 1 do
  begin
    FImages.Add( Images[Index] );
  end;
end;

end.
