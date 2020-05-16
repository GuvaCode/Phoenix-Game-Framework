unit uDesigner;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  phxTypes,
  phxDevice,
  phxGraphics,
  phxDraw,
  phxCanvas,
  phxTexture,
  phxSkin,
  phxFont,
  phxSimpleGui,

  uActions,
  uProject,
  uPalette,
  uInspector;

type
  TFrmDesigner = class(TFrame)
    ScrollBox1: TScrollBox;
    PHXDraw1: TPHXDraw;

    procedure PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PHXDraw1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,   Y: Integer);
    procedure PHXDraw1MouseUp(Sender: TObject; Button: TMouseButton;   Shift: TShiftState; X, Y: Integer);
  private
    FProject: TGuiProject;
    FControl: TGuiControl;

    Canvas: TPHXCanvas;
    Gui   : TPHXSimpleGUI;

    LineTexture: TPHXTexture;

    Skin    : TPHXSkin;
    SkinName: String;
    Font    : TPHXFont;
    FontName: String;

    Background: TPHXTexture;
    BackgroundName: String;

    MousePos: TPoint;

    DragPosition: TPoint;
    DragControl : TGuiControl;

    procedure SetProject(const Value: TGuiProject);
    procedure SetControl(const Value: TGuiControl);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;


    property Project: TGuiProject read FProject write SetProject;
    property Control: TGuiControl read FControl write SetControl;
  end;

var FrmDesigner: TFrmDesigner;

implementation

{$R *.dfm}

const TextureData : {$I 'Line.inc'}

// TFrmDesigner
//==============================================================================
constructor TFrmDesigner.Create(AOwner: TComponent);
begin
  inherited;

  PHXDraw1.Width := Screen.Width;
  PHXDraw1.Height:= Screen.Height;

  SetProject(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:= FrmPalette.IsDragSource(Source);
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1DragDrop(Sender, Source: TObject; X, Y: Integer);
var Control: TGuiControl;
begin
  if FrmPalette.IsDragSource(Source) then
  begin
    Control:= FrmPalette.CreateControl(Project);

    if Assigned(Control) then
    begin
      Control.X:= X;
      Control.Y:= Y;

      Project.Controls.Add(Control);

      ModActions.Control:= Control;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
begin
  Canvas:= Device.CreateCanvas;
  Background:= Device.CreateTexture;

  LineTexture:= Device.CreateTexture;
  LineTexture.Import(2, 2, pfRGBA, @TextureData);

  Skin:= TPHXSkin.Create(Device, Canvas);
 // Skin.LoadSkin('D:\Program\Programmering\Phoenix\Design\Demos\_Binary\Content\Default.phxskn');

  Font:= TPHXFont.Create(Device, Canvas);
 // Font.LoadFromFile('D:\Program\Programmering\Phoenix\Design\Demos\_Binary\Content\Calibri12.phxfnt');

  Gui:= TPHXSimpleGUI.Create(Canvas, Skin, Font);
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Control: TGuiControl;
begin
  if Assigned(Project) then
  begin
    if Button = TMouseButton.mbLeft then
    begin
      Control:= Project.ControlAt(X,Y);

      if Assigned(Control) then
      begin
        DragControl   := Control;
        DragPosition.X:= Control.X;
        DragPosition.Y:= Control.Y;
      end;

      ModActions.Control:= Control;
    end;
  end;
  MousePos.X:= X;
  MousePos.Y:= Y;
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(DragControl) then
  begin
    DragControl.X:= DragPosition.X + (X - MousePos.X);
    DragControl.Y:= DragPosition.Y + (Y - MousePos.Y);
  end;
 // DownPosition:= Point(X,Y);
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DragControl:= nil;
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
var R,G,B: Single;
var Rect: TRectf;
var Coord: TRectf;
begin
  if Assigned(Project) then
  begin
    Device.SetClearColor(clrWhite);
  end else
  begin
    R:= GetRValue(Vcl.Graphics.ColorToRGB(clBtnFace)) / 255;
    G:= GetGValue(Vcl.Graphics.ColorToRGB(clBtnFace)) / 255;
    B:= GetBValue(Vcl.Graphics.ColorToRGB(clBtnFace)) / 255;

    Device.SetClearColor(R, G, B);
  end;

  Device.Clear;

  if not Background.Empty then
  begin
    Canvas.Texture:= Background;
    Canvas.Background(Device.Width, Device.Height);
  end;


  if Assigned(Project) then
  begin
    Project.Render(Gui);
  end;

 // Label1.Caption:= IntToStr(Gui.Focused);

  if Assigned(FControl) then
  begin
    Rect.Left  := FControl.ControlRect.Left;
    Rect.Top   := FControl.ControlRect.Top;
    Rect.Right := FControl.ControlRect.Right;
    Rect.Bottom:= FControl.ControlRect.Bottom;

    Coord.Left  := 0.0;
    Coord.Top   := 0.0;
    Coord.Right := (FControl.ControlRect.Width  / 2);
    Coord.Bottom:= (FControl.ControlRect.Height / 2);

    Canvas.Texture:= LineTexture;
    Canvas.Color  := clrMaroon;
    Canvas.Rectangle(Rect, Coord);

  //  Canvas.Rectangle(Rect.Left-1, Rect.Top-1, Rect.Right+1, Rect.Bottom+1);
//    Canvas.FilledRectangle(Rect.Left-1, Rect.Top-1, Rect.Right+1, Rect.Bottom+1);
    Canvas.Color  := clrWhite;
  end;

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.SetParent(AParent: TWinControl);
begin
  inherited;

  if Assigned(AParent) then
  begin
    PHXDraw1.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.SetProject(const Value: TGuiProject);
var Dir: String;
var Name: String;
begin
  Dir:= GetCurrentDir;

  if FProject <> Value then
  begin
    SetControl(nil);

    FProject := Value;
  end;

  if Assigned(Project) then
  begin
    SetCurrentDir( ExtractFilePath(Project.Name) );

    if BackgroundName <> FProject.Settings.Background then
    begin
      BackgroundName:= FProject.Settings.Background;

      Name:= ExpandFileName(FProject.Settings.Background);

      if FileExists(Name) then
      begin
        Background.LoadTexture(Name);
      end;
    end;

    if SkinName <> FProject.Settings.Skin then
    begin
      SkinName:= FProject.Settings.Skin;

      Name:= ExpandFileName(FProject.Settings.Skin);

      if FileExists(Name) then
      begin
        Skin.LoadFromFile(Name);
      end;
    end;

    if FontName <> FProject.Settings.Font then
    begin
      FontName:= FProject.Settings.Font;

      Name:= ExpandFileName(FProject.Settings.Font);

      if FileExists(Name) then
      begin
        Font.LoadFromFile(Name);
      end;
    end;
  end;

  SetCurrentDir(Dir)
end;

//------------------------------------------------------------------------------
procedure TFrmDesigner.SetControl(const Value: TGuiControl);
begin
  FControl:= Value;
end;

end.
