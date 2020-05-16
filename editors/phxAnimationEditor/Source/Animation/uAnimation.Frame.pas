unit uAnimation.Frame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,
  Vcl.Mask, JvExMask, JvSpin, JvExStdCtrls, JvCombobox,

  phxImage;

type

//------------------------------------------------------------------------------
TFrmAnimationFrame = class(TFrame)
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edFrameIndex: TEdit;
    edFrameTime: TJvSpinEdit;
    edFramePatternName: TJvComboBox;
    edFrameName: TEdit;
    Label1: TLabel;
    edFramePattern: TJvSpinEdit;
    procedure edFramePatternChange(Sender: TObject);
    procedure edFrameTimeChange(Sender: TObject);
    procedure edFrameNameChange(Sender: TObject);
  private
    FFrame: Integer;
    FAnimation: TPHXAnimation;

    function PatternName(const Index: Integer): ShortString;

    procedure EnableEditors(Enabled: Boolean);

    procedure SetAnimation(const Value: TPHXAnimation);
    procedure SetFrame(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    // The animation
    property Animation: TPHXAnimation read FAnimation write SetAnimation;
    // The animation frame index
    property Frame: Integer read FFrame write SetFrame;
  end;

implementation

{$R *.dfm}

uses uActions;

// TFrmAnimationFrame
//==============================================================================
constructor TFrmAnimationFrame.Create(AOwner: TComponent);
begin
  inherited;
  SetAnimation(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.SetAnimation(const Value: TPHXAnimation);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;

    SetFrame(-1);
  end else
  begin
    SetFrame(FFrame);
  end;
end;

//------------------------------------------------------------------------------
function TFrmAnimationFrame.PatternName(const Index: Integer): ShortString;
begin
  if Assigned(Animation) and Assigned(Animation.Image) and ( Index >= 0) and (Index < Animation.Image.Patterns.Count) then
  begin
     Result:= Animation.Image.Patterns[Index].Name;
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.SetFrame(const Value: Integer);
var Index: Integer;
begin
  FFrame := Value;

  if Assigned(FAnimation) and (FFrame >= 0) then
  begin
    EnableEditors(True);

    edFramePatternName.Items.Clear;
    if Assigned(FAnimation.Image) then
    begin
      for Index := 0 to FAnimation.Image.Patterns.Count - 1 do
      begin
        edFramePatternName.Items.Add( String(FAnimation.Image.Patterns.List^[Index].Name) );
      end;
    end;

    edFrameIndex  .Text  := IntToStr(FFrame);
    edFrameName   .Text  := String(     Animation.Frames.List^[FFrame].Name);
    edFrameTime   .Value :=             Animation.Frames.List^[FFrame].Time * 1000;
    edFramePattern.Value :=             Animation.Frames.List^[FFrame].Pattern;
  end else
  begin
    EnableEditors(False);

    edFramePatternName.Items.Clear;

    edFrameIndex  .Text  := '';
    edFrameName   .Text  := '';
    edFrameTime   .Value := 0;
    edFramePattern.Value := 0;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.EnableEditors(Enabled: Boolean);
const EnabledColors: array [Boolean] of TColor = (clBtnFace, clWindow);
begin

  edFrameName.Enabled := Enabled;
  edFrameName.Color := EnabledColors[Enabled];

  edFrameTime.Enabled := Enabled;
  edFrameTime.Color := EnabledColors[Enabled];

  edFramePattern.Enabled := Enabled;
  edFramePattern.Color := EnabledColors[Enabled];

  if Enabled then
  begin
    edFrameName   .OnChange:= edFrameNameChange;
    edFrameTime   .OnChange:= edFrameTimeChange;
    edFramePattern.OnChange:= edFramePatternChange;
  end else
  begin
    edFrameName   .OnChange:= nil;
    edFrameTime   .OnChange:= nil;
    edFramePattern.OnChange:= nil;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.edFrameNameChange(Sender: TObject);
var Value: ShortString;
begin
  Value:= ShortString(edFrameName.Text);

  if Animation.Frames.List^[FFrame].Name <> Value then
  begin
    Animation.Frames.List^[FFrame].Name := Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.edFrameTimeChange(Sender: TObject);
var Value: Single;
begin
  Value:= edFrameTime.Value / 1000;

  if Animation.Frames.List^[FFrame].Time <> Value then
  begin
    Animation.Frames.List^[FFrame].Time := Value;

    ModActions.Document.Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationFrame.edFramePatternChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edFramePattern.Value);

  if Animation.Frames.List^[FFrame].Pattern <> Value then
  begin
    Animation.Frames.List^[FFrame].Pattern := Value;
    Animation.Frames.List^[FFrame].Name    := PatternName(Value);

    edFrameName.OnChange:= nil;
    edFrameName.Text    := String(Animation.Frames.List^[FFrame].Name);
    edFrameName.OnChange:= edFrameNameChange;

    ModActions.Document.Changed;
  end;
end;





end.
