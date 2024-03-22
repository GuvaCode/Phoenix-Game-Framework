unit uAnimation.Preview;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls,
  ActnList, ImgList, ComCtrls, ToolWin, StdCtrls,

  phxTypes,
  phxGraphicsEx,

  phxImage,
  phxImageEx;

type

  { TFrmAnimationPreview }

  TFrmAnimationPreview = class(TFrame)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    btnPlay: TToolButton;
    btnPause: TToolButton;
    btnStop: TToolButton;
    TrackBar1: TTrackBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    actPlay: TAction;
    actPause: TAction;
    actStop: TAction;
    Timer1: TTimer;
    Panel2: TPanel;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure actPlayExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actAnimationUpdate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FAnimation: TPHXAnimation;
    FImage    : TPHXImage;
    FBuffer    : TBitmap;
    FBackground: TBitmap;

    FTimerFrequency: Int64;
    FTimerTicks    : Int64;

    AnimationState  : TPHXAnimationState;

    function GetDeltaTime: Single;

    procedure DrawPattern(const Rect: TRecti; const PatternIndex: Integer);

    procedure SetAnimation(const Value: TPHXAnimation);
  public
    constructor Create(AOwner: TComponent); override;

    property Animation: TPHXAnimation read FAnimation write SetAnimation;
    property Image: TPHXImage read FImage;
  end;

implementation

{$R *.lfm}

// TFrmAnimationPreview
//==============================================================================
constructor TFrmAnimationPreview.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);

  Panel2.DoubleBuffered:= True;

  { #todo : Fix Me EpikTmier? }
  {
  QueryPerformanceFrequency(FTimerFrequency);
  QueryPerformanceCounter(FTimerTicks);
 }
end;



//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.SetAnimation(const Value: TPHXAnimation);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    if Assigned(FAnimation) then
    begin
     { #todo : Fix ME } // Animation.Reset(AnimationState);
    end else
    begin
      AnimationState.Active := False;
      AnimationState.Time   := 0;
      AnimationState.Frame  := -1;
      AnimationState.Pattern:= -1;
    end;
  end;

  if Assigned(FAnimation) then
  begin
    FImage:= FAnimation.Image;

    Timer1.Enabled:= True;
  end else
  begin
    FImage:= nil;

    Timer1.Enabled:= False;
  end;

  if Assigned(Image) then
  begin
   //////////
    /////
    Image.Draw(FBuffer, Graphics.ColorToRGB(clWindow));
  end else
  begin
    FBuffer.Width := 0;
    FBuffer.Height:= 0;
  end;


  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.actAnimationUpdate(Sender: TObject);
begin
  actPlay .Enabled:= Assigned(Animation) and (AnimationState.Active = False);
  actStop .Enabled:= Assigned(Animation) and (AnimationState.Active = True);
  actPause.Enabled:= Assigned(Animation) and (AnimationState.Active = True);
end;

procedure TFrmAnimationPreview.TrackBar1Change(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.actPlayExecute(Sender: TObject);
begin
 { #todo : Fix ME } // Animation.Reset(AnimationState);
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.actPauseExecute(Sender: TObject);
begin
  AnimationState.Active:= not AnimationState.Active;

  btnPause.Down:= AnimationState.Active;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.actStopExecute(Sender: TObject);
begin
 { #todo : Fix ME } // Animation.Reset(AnimationState);

  AnimationState.Active := False;

  TrackBar1.Position:= 0;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.Timer1Timer(Sender: TObject);
var Index: Integer;
var Delta: Single;
begin
  Delta:= GetDeltaTime;

  if Assigned(Animation) then
  begin
    Index := AnimationState.Frame;

    // Running
    if AnimationState.Active then
    begin
    { #todo : Fix ME } //  Animation.Update(AnimationState, Delta);

      if Index <> AnimationState.Frame then
      begin

        TrackBar1.Min      := 0;
        TrackBar1.Max      := Animation.Frames.Count - 1;
        TrackBar1.Position := AnimationState.Frame;

        PaintBox1.Invalidate;
      end;
    end else
    // Paused
    begin
      if AnimationState.Frame <> TrackBar1.Position then
      begin

        AnimationState.Frame := TrackBar1.Position;

        if (AnimationState.Frame >= 0) and (AnimationState.Frame < Animation.Frames.Count) then
        begin
          AnimationState.Pattern := Animation.Frames[AnimationState.Frame].Pattern;
        end;
        PaintBox1.Invalidate;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.DrawPattern(const Rect: TRecti; const PatternIndex: Integer);
var Pattern: TPHXPattern;
var X,Y: Integer;
begin
  if Assigned(Image) and ( PatternIndex >= 0) and (PatternIndex < Image.Patterns.Count) then
  begin
    Pattern:= Image.Patterns.List^[PatternIndex];

    X:= Rect.Left + ((Rect.Right  - Rect.Left) - Pattern.Width ) div 2;
    Y:= Rect.Top  + ((Rect.Bottom - Rect.Top ) - Pattern.Height) div 2;

    //////////
    Animation.Image.DrawPattern(PaintBox1.Canvas, FBuffer, X,Y, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationPreview.PaintBox1Paint(Sender: TObject);
var Pattern: TPHXPattern;
var Rect: TRecti;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Color:= clWindow;
    Brush.Style:= bsSolid;

    FillRect(PaintBox1.ClientRect);

    if Animation = nil then
    begin
      Exit;
    end;

    if Assigned(Image) and (AnimationState.Pattern >= 0) and (AnimationState.Pattern < Image.Patterns.Count) then
    begin
      Pattern:= Image.Patterns.List^[AnimationState.Pattern];

      Rect.Left  := 2;
      Rect.Top   := 2;
      Rect.Right := PaintBox1.Width - 2;
      Rect.Bottom:= PaintBox1.Height - 2;

      DrawPattern( Rect, AnimationState.Pattern);
    end else
    begin

      Rect.Left  := 0;
      Rect.Top   := 0;
      Rect.Right := PaintBox1.Width;
      Rect.Bottom:= PaintBox1.Height;

      //DrawEmpty(Rect);
    end;
       {
    // Draw the name of the animation frame
    if (AnimationState.Frame >= 0) and (  AnimationState.Frame < Animation.Frames.Count) then
    begin
      Frame:= Animation.Frames[AnimationState.Frame];

      X:= 4;//X + 2;
      Y:= 4;//Y - PaintBox1.Canvas.TextHeight(String(Frame.Name));

      PaintBox1.Canvas.TextOut(X, Y, 'Frame: ' + String(Frame.Name));
    end;
     }

  end;
end;





//------------------------------------------------------------------------------
function TFrmAnimationPreview.GetDeltaTime: Single;
var Ticks: Int64;
begin
  { #todo : Fix ME }
  {
  QueryPerformanceCounter(Ticks);

  Result := (Ticks - FTimerTicks) / FTimerFrequency;

  FTimerTicks:= Ticks;
  }
end;




end.
