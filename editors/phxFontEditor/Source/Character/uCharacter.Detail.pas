unit uCharacter.Detail;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Spin, ExtCtrls, Mask,

  phxGraphics,
  phxGraphicsEx,

  phxFont;

type
  TFrmCharacter = class(TFrame)
    Panel6: TPanel;
    pbPreview: TPaintBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    edXOffset: TSpinEdit;
    edYOffset: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    edXAdvance: TSpinEdit;
    edWidth: TSpinEdit;
    edY: TSpinEdit;
    edX: TSpinEdit;
    edHeight: TSpinEdit;
    GroupBox4: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    edIdent: TSpinEdit;
    edText: TEdit;
    procedure pbPreviewPaint(Sender: TObject);
    procedure edCharacterChange(Sender: TObject);
  private
    FFont      : TPHXFont;
    FBuffer    : TBitmap;
    FBackground: TBitmap;
    FCharacter : TPHXCharacter;
    FOnChange: TNotifyEvent;

    procedure Changed;

    procedure BindEvents(const Enabled: Boolean);

    procedure SetCharacter(const Value: TPHXCharacter);
    procedure SetFont(const Value: TPHXFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawCharacter(Bitmap: TBitmap; const Character: TPHXCharacter);

    procedure EnableControls(const Enabled: Boolean);

    property Font: TPHXFont read FFont write SetFont;

    property Character: TPHXCharacter read FCharacter write SetCharacter;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

uses uActions;

//uses phxGraphicUtils;

(*
  // The ascii code of the character.
  ID      : Cardinal;
  // The x-position of the character in the texture.
  X       : Integer;
  // The y-position of the character in the texture.
  Y       : Integer;
  // The width of the character in the texture.
  Width   : Integer;
  // The height of the character in the texture.
  Height  : Integer;
  // The x-offset of the character relative to the current raster.
  XOffset : Integer;
  // The y-offset of the character relative to the current raster.
  YOffset : Integer;
  // The number of pixels to move the raster in the horisontal plane.
  XAdvance: Integer;
*)


//const BACKGROUND_COLOR = $00D88B5D;

const COLOR_BACKGROUND1 = $00D8D8D8;
//const COLOR_BACKGROUND2 = $00C0C0C0;

// TFrmCharacter
//------------------------------------------------------------------------------
constructor TFrmCharacter.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(8);

end;

//------------------------------------------------------------------------------
destructor TFrmCharacter.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmCharacter.Changed;
begin
  ModActions.Document.Changed;

  if Assigned(OnChange) then OnChange(Self);

  pbPreview.Invalidate;
end;

{$REGION 'Editor Events'}



{$ENDREGION}

//------------------------------------------------------------------------------
procedure TFrmCharacter.SetCharacter(const Value: TPHXCharacter);
begin
  FCharacter := Value;

  BindEvents(False);
  edIdent   .Value:= Character.ID;
  edX       .Value:= Character.X;
  edY       .Value:= Character.Y;
  edWidth   .Value:= Character.Width;
  edHeight  .Value:= Character.Height;
  edXOffset .Value:= Character.Offset.X;
  edYOffset .Value:= Character.Offset.Y;
  edXAdvance.Value:= Character.Advance;
  edText    .Text := Chr(Character.ID);

  // This character is used for wrapping on words
 // cfWrap,
  //// This character is not allowed on the start of wrapped lines
  //cfNotStart,
  // This character is not allowed on the end of wrapped lines
//  cfNotEnd


  BindEvents(True);

  pbPreview.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmCharacter.edCharacterChange(Sender: TObject);
begin
  FCharacter.ID      := Round(edIdent   .Value);
  FCharacter.X       := Round(edX       .Value);
  FCharacter.Y       := Round(edY       .Value);
  FCharacter.Width   := Round(edWidth   .Value);
  FCharacter.Height  := Round(edHeight  .Value);
  FCharacter.Offset.X:= Round(edXOffset .Value);
  FCharacter.Offset.Y:= Round(edYOffset .Value);
  FCharacter.Advance := Round(edXAdvance.Value);

  edText.Text := Chr(Character.ID);

  if Assigned(OnChange) then OnChange(Self);

  pbPreview.Invalidate;
end;




//------------------------------------------------------------------------------
procedure TFrmCharacter.BindEvents(const Enabled: Boolean);
var EnabledEvents: Array[Boolean] of TNotifyEvent;
begin
  EnabledEvents[True ]:= edCharacterChange;
  EnabledEvents[False]:= nil;

  edIdent   .OnChange:= EnabledEvents[Enabled];
  edX       .OnChange:= EnabledEvents[Enabled];
  edY       .OnChange:= EnabledEvents[Enabled];
  edWidth   .OnChange:= EnabledEvents[Enabled];
  edHeight  .OnChange:= EnabledEvents[Enabled];
  edXOffset .OnChange:= EnabledEvents[Enabled];
  edYOffset .OnChange:= EnabledEvents[Enabled];
  edXAdvance.OnChange:= EnabledEvents[Enabled];
end;


//------------------------------------------------------------------------------
procedure TFrmCharacter.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin

  edIdent   .Color:= EnabledColors[Enabled];
  edX       .Color:= EnabledColors[Enabled];
  edY       .Color:= EnabledColors[Enabled];
  edWidth   .Color:= EnabledColors[Enabled];
  edHeight  .Color:= EnabledColors[Enabled];
  edXOffset .Color:= EnabledColors[Enabled];
  edYOffset .Color:= EnabledColors[Enabled];
  edXAdvance.Color:= EnabledColors[Enabled];

  edIdent   .Enabled:= Enabled;
  edX       .Enabled:= Enabled;
  edY       .Enabled:= Enabled;
  edWidth   .Enabled:= Enabled;
  edHeight  .Enabled:= Enabled;
  edXOffset .Enabled:= Enabled;
  edYOffset .Enabled:= Enabled;
  edXAdvance.Enabled:= Enabled;

  //btnSetOffsetY.Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmCharacter.SetFont(const Value: TPHXFont);
begin
  FFont := Value;

  if Assigned(FFont) then
  begin
    DrawTexture(Font.Texture, FBuffer, $00C0C0C0);
  end else
  begin
    FBuffer.Width:= 0;
    FBuffer.Height:= 0;
  end;

  pbPreview.Invalidate;
end;



//------------------------------------------------------------------------------
procedure TFrmCharacter.DrawCharacter(Bitmap: TBitmap; const Character: TPHXCharacter);
var DstRect: TRect;
var SrcRect: TRect;
begin
  DstRect.Left  := 0;
  DstRect.Top   := 0;
  DstRect.Right := Character.Width;
  DstRect.Bottom:= Character.Height;

  SrcRect.Left  := Character.X;
  SrcRect.Top   := Character.Y;
  SrcRect.Right := Character.X + Character.Width;
  SrcRect.Bottom:= Character.Y + Character.Height;

  Bitmap.Width := Character.Width;
  Bitmap.Height:= Character.Height;

  Bitmap.Canvas.CopyRect(DstRect, FBuffer.Canvas, SrcRect);
end;

const Zoom: Single = 4;

//------------------------------------------------------------------------------
procedure TFrmCharacter.pbPreviewPaint(Sender: TObject);
var DstRect: TRect;
var Temp   : TBitmap;
var X,Y,P  : Integer;
begin
  with pbPreview.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= $00C0C0C0;

    FillRect(ClipRect);

    if Assigned(FFont) then
    begin
      Temp:= TBitmap.Create;
      try
        DrawCharacter(Temp, FCharacter);

        X:= (pbPreview.Width  - Round(Temp.Width  * Zoom)                                  ) div 2;
        Y:= (pbPreview.Height - Round(Temp.Height * Zoom){ + Round(Character.YOffset * Zoom)}) div 2;

        DstRect.Left  := X;
        DstRect.Top   := Y;
        DstRect.Right := X + Round(Temp.Width  * Zoom);
        DstRect.Bottom:= Y + Round(Temp.Height * Zoom);

        StretchDraw( DstRect, Temp);

        // Draw character bounds
        Pen.Color:= $00808080;
        MoveTo(DstRect.Left , DstRect.Top);
        LineTo(DstRect.Right, DstRect.Top);
        LineTo(DstRect.Right, DstRect.Bottom);
        LineTo(DstRect.Left , DstRect.Bottom);
        LineTo(DstRect.Left , DstRect.Top);

         // Draw vertical offset
        P:= Y + Round(Character.Offset.Y * Zoom);
        Pen.Color:= $00909090;
        MoveTo(DstRect.Left , P);
        LineTo(DstRect.Right, P);

       // Draw Ascent
        P:= Y + Round(Character.Offset.Y * Zoom) + Round(FFont.Metric.Ascent * Zoom);
        Pen.Color:= clGreen;
        MoveTo(DstRect.Left , P);
        LineTo(DstRect.Right, P);

        // Draw Height
        P:= Y + Round(Character.Offset.Y * Zoom) + Round(FFont.Metric.Ascent * Zoom) + Round(FFont.Metric.Descent * Zoom);// +  Round(FFont.Metric.Height * Zoom);
        Pen.Color:= clBlack;
        MoveTo(DstRect.Left , P);
        LineTo(DstRect.Right, P);


        // Draw X-Offset
        P:= X + Round(Character.Offset.X * Zoom);
        Pen.Color:= clRed;
        MoveTo(P, DstRect.Top);
        LineTo(P, DstRect.Bottom+1);

        // Draw X-Advance
        P:= X + Round(Character.Offset.X * Zoom) + Round(Character.Advance * Zoom);
        Pen.Color:= clBlue;
        MoveTo(P, DstRect.Top);
        LineTo(P, DstRect.Bottom+1);

      finally
        Temp.Free;
      end;
    end;
  end;
end;

end.
