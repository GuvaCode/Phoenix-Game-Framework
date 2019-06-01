////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxSimpleGUI;
//< @include(docs/phxSimpleGUI.txt)

{$I phxConfig.inc}

interface

uses
  Classes, SysUtils,

  phxTypes,
  phxMath,
  phxEvents,
  phxDevice,
  phxCanvas,
  phxSkin,
  phxFont,
  phxInput;

type

// Identifier for gui controls
TGUIIdent = Integer;

// Direction for the progress bar
//------------------------------------------------------------------------------
TProgressDirection = (
  // Draw the bar horisontaly
  pdHorisontal,
  // Draw the bar verticaly
  pdVertical
);

// @include(docs/phxSimpleGUI.TPHXSimpleGUI.txt)
//------------------------------------------------------------------------------
TPHXSimpleGUI = class
  private
    FCanvas: TPHXCanvas;
    FSkin  : TPHXSkin;
    FFont  : TPHXFont;
    FCount : Integer;
    FIdent : TGUIIdent;
    FAlpha : Single;
    FTime  : Single;

    FHover  : TGUIIdent;
    FFocused: TGUIIdent;
    FPressed: TGUIIdent;

    FPosition: TVector2i;

    // True between Prepare and Finish
    FActive: Boolean;

    // Left mouse button is pressed
    MousePressed: Boolean;
    // Current mouse position
    MousePos: TVector2f;
    // Mouse down position
    MouseDown: TVector2f;

    KeyPressed  : TPHXVirtualKey;
    KeyReleased : TPHXVirtualKey;
    KeyText     : WideChar;


    // Return the widget rectangle for a widget
    function GetWidgetRect(const X, Y, Width, Height: Integer): TRectf; inline;
    function GetWidgetSkin(Part: TPHXThemedPart): TPHXSkinElement;

    // Handles a phoenix event
    procedure EventHandler(Sender: TObject; const Event: TPHXEvent);
    // Handles forcus for a control
    procedure FocusHandler(const Control: TGUIIdent; const Bounds: TRectf);

    // The current control ident
    property Ident: TGUIIdent read FIdent;
  public
    // Creates a new gui
    // @param(ACanvas The canvas to render to)
    // @param(ASkin The skin to use for the gui)
    // @param(AFont the font to use for the gui)
    constructor Create(ACanvas: TPHXCanvas; ASkin: TPHXSkin; AFont: TPHXFont);
    // Destroys the gui
    destructor Destroy; override;

    // Update the gui
    // @param(DeltaTime The elapsed time since the last update)
    procedure Update(const DeltaTime: Single);

    // Call this before rendering any widgets
    procedure Prepare;
    // Call this after the rendering is completed
    procedure Finish;


    // Render a window widget
    // @param(X The horisontal position of the window)
    // @param(Y The vertical position of the window)
    // @param(Width The horisontal size of the window)
    // @param(Height The vertical size of the window)
    // @returns(True if the window is clicked)
    function Window(const X, Y, Width, Height: Integer): Boolean;

    // Render a panel widget
    // @param(X The horisontal position of the panel)
    // @param(Y The vertical position of the panel)
    // @param(Width The horisontal size of the panel)
    // @param(Height The vertical size of the panel)
    // @returns(True if the panel is clicked)
    function Panel(const X, Y, Width, Height: Integer): Boolean;

    // Render a text widget
    // @param(X The horisontal position of the text widet)
    // @param(Y The vertical position of the text widet)
    // @param(Width The horisontal size of the text widet)
    // @param(Height The vertical size of the text widet)
    // @param(Text The text to show)
    // @returns(True if the text widget is clicked)
    function Text(const X, Y, Width, Height: Integer; const Text: String): Boolean; overload;

    // Render a text widget with a custom font
    // @param(X The horisontal position of the text widet)
    // @param(Y The vertical position of the text widet)
    // @param(Width The horisontal size of the text widet)
    // @param(Height The vertical size of the text widet)
    // @param(Text The text to show)
    // @param(Font The font for the widget)
    // @returns(True if the text widget is clicked)
    function Text(const X, Y, Width, Height: Integer; const Text: String; const Font: TPHXFont): Boolean; overload;

    // Render a button widget
    // @param(X The horisontal position of the button)
    // @param(Y The vertical position of the button)
    // @param(Width The horisontal size of the button)
    // @param(Height The vertical size of the button)
    // @param(Text The text to show)
    // @returns(If the panel is pressed)
    function Button(const X, Y, Width, Height: Integer; const Text: String): Boolean;

    // Render a edit widget
    // @param(X The horisontal position of the edit)
    // @param(Y The vertical position of the edit)
    // @param(Width The horisontal size of the edit)
    // @param(Height The vertical size of the edit)
    // @param(Text The text to show and edit)
    // @returns(True when the text is changed)
    function Edit(const X, Y, Width, Height: Integer; var Text: String): Boolean;

    // Render a checkbox widget
    // @param(X The horisontal position of the checkbox)
    // @param(Y The vertical position of the checkbox)
    // @param(Width The horisontal size of the checkbox)
    // @param(Height The vertical size of the checkbox)
    // @param(Text The text to render)
    // @param(Checked The value to toggle)
    // @returns(True when checked variable is changed)
    function CheckBox(const X, Y, Width, Height: Integer; const Text: String; var Checked: Boolean): Boolean;

    // Render a slider widget
    // @param(X The horisontal position of the slider)
    // @param(Y The vertical position of the slider)
    // @param(Width The horisontal size of the slider)
    // @param(Height The vertical size of the slider)
    // @param(Min The minimum value)
    // @param(Max The maximum value)
    // @param(Value The current value)
    // @returns(true when the value is changed.)
    function Slider(const X, Y, Width, Height: Integer; const Min, Max: Single; var Value: Single): Boolean;

    // Render a progressbar widget
    // @param(X The horisontal position of the progressbar)
    // @param(Y The vertical position of the progressbar)
    // @param(Width The horisontal size of the progressbar)
    // @param(Height The vertical size of the progressbar)
    // @param(Progress The progress between 0.0 and 1.0)
    // @param(Direction The direction of the progress bar)
    // @returns(True if the progressbar is clicked)
    function ProgressBar(const X, Y, Width, Height: Integer; const Progress: Single; const Direction: TProgressDirection = pdHorisontal): Boolean;

    // Render a selector widget
    // @param(X The horisontal position of the slider)
    // @param(Y The vertical position of the slider)
    // @param(Width The horisontal size of the slider)
    // @param(Height The vertical size of the slider)
    // @param(Values List of values to select from)
    // @param(Selected Index of the selected item)
    // @returns(True when the selected index was changed.)
    function Selector(const X, Y, Width, Height: Integer; const Values: TStrings; var Selected: Integer): Boolean;


    // The canvas
    property Canvas: TPHXCanvas read FCanvas write FCanvas;
     // The skin to use
    property Skin: TPHXSkin read FSkin write FSkin;
    // The font to use
    property Font: TPHXFont read FFont write FFont;
    // Returns the number of rendered widgets
    property Count: Integer read FCount;
    // Return the ident of the hover control
    property Hover: TGUIIdent read FHover;
    // Return the ident of the focused control
    property Focused: TGUIIdent read FFocused;
    // Return the ident of the pressed control
    property Pressed: TGUIIdent read FPressed;

    // Changes the position of the gui on the screen
    property Position: TVector2i read FPosition write FPosition;
  end;


implementation

resourcestring
  SNotActive = 'Tou must call Prepare before rendering any widgets';


// TPHXIntermediateGUI
//==============================================================================
constructor TPHXSimpleGUI.Create(ACanvas: TPHXCanvas; ASkin: TPHXSkin; AFont: TPHXFont);
begin
  FCanvas:= ACanvas;
  FSkin  := ASkin;
  FFont  := AFont;
  FAlpha := 1.0;

  TPHXEvents.AddListener(EventHandler);
end;


//------------------------------------------------------------------------------
destructor TPHXSimpleGUI.Destroy;
begin
  TPHXEvents.RemoveListener(EventHandler);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSimpleGUI.Update(const DeltaTime: Single);
begin
  FTime:= FTime + DeltaTime;
end;

//------------------------------------------------------------------------------
procedure TPHXSimpleGUI.EventHandler(Sender: TObject; const Event: TPHXEvent);
begin
  case Event.Event of
    PHX_MOUSE_PRESSED:
    begin
      MousePressed := (Event.Mouse.Button = mbLeft);

      MouseDown.X:= Event.Mouse.X;
      MouseDown.Y:= Event.Mouse.Y;

      FPressed:= 0;
      FFocused:= 0;
    end;
    PHX_MOUSE_RELEASED:
    begin
      MousePressed := False;
    end;
    PHX_MOUSE_MOVED:
    begin
      MousePos.X:= Event.Mouse.X;
      MousePos.Y:= Event.Mouse.Y;
    end;
    PHX_KEY_PRESSED:
    begin
      KeyPressed := Event.Keyboard.Key;
      KeyReleased:= VK_UNKNOWN;

      if KeyPressed = VK_TAB then
      begin
        Inc(FFocused);
        if Focused > FCount then FFocused:= 1;
      end;

    end;
    PHX_KEY_RELEASED:
    begin
      KeyPressed := VK_UNKNOWN;
      KeyReleased:= Event.Keyboard.Key;
    end;
    PHX_KEY_CHARACTER:
    begin
      KeyText:= Event.Keyboard.Char;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSimpleGUI.FocusHandler(const Control: TGUIIdent; const Bounds: TRectf);
begin
  if PointInRect(MousePos, Bounds) then
  begin
    FHover:= Control;

    if MousePressed then
    begin
      FPressed:= Control;
      FFocused:= Control;

     // MousePressed:= -1;
    end;
  end;

  (*
  if Focused = PHXIMGUI_NONE  then
  begin
    FFocused:= Ident;
  end;

  // Handle control input
  if Focused = Ident then
  begin
    if (MouseReleased = PHX_MOUSE_BUTTON_1) and PointInRect(MousePos, Bounds) then
    begin
      FPressed:= PHXIMGUI_NONE;
    end;
    // Tabbing support
    if KeyPressed = VK_TAB then
    begin
      FFocused:= PHXIMGUI_NONE;

      KeyPressed:= VK_UNKNOWN;
    end;
  end;
  *)
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.GetWidgetRect(const X, Y, Width, Height: Integer): TRectf;
begin
  Result.Left  := FPosition.X + X;
  Result.Right := FPosition.X + X + Width;
  Result.Top   := FPosition.Y + Y;
  Result.Bottom:= FPosition.Y + Y + Height;
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.GetWidgetSkin(Part: TPHXThemedPart): TPHXSkinElement;
begin
  if Assigned(FSkin) then
  begin
    Result:= FSkin.Parts[Part];
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSimpleGUI.Prepare;
begin
  Assert(FActive = False, 'You must call Finish after Prepare');

  FCount:= FIdent;
  FIdent:= 0;
  FHover:= 0;

  Canvas.Flush;

  FActive:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSimpleGUI.Finish;
begin
  Assert(FActive = True, 'You must call Prepare before Finish');

  if not MousePressed then
  begin
    FPressed:=0;
  end;

  FActive:= False;
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Window(const X, Y, Width, Height: Integer): Boolean;
var Rect: TRectf;
var Skin: TPHXSkinElement;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    Result:= True;
  end;

  Skin:= GetWidgetSkin(teWindowNormal);

  if Assigned(Skin) then
  begin
    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Panel(const X, Y, Width, Height: Integer): Boolean;
var Rect : TRectf;
var Skin: TPHXSkinElement;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    Result:= True;
  end;

  Skin:= GetWidgetSkin(tePanelNormal);

  if Assigned(FSkin) then
  begin
    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Text(const X, Y, Width, Height: Integer; const Text: String): Boolean;
begin
  Assert(FActive, SNotActive);

  Result:= Self.Text(X, Y, Width, Height, Text, FFont);
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Text(const X, Y, Width, Height: Integer; const Text: String; const Font: TPHXFont): Boolean;
var Rect : TRectf;
var Color: TColor4f;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    Result:= True;
  end;

  Color:= clrWhite;
  Color.Alpha:= FAlpha;

  Font.TextOut(Rect, Text, taLeft, Color);
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Button(const X, Y, Width, Height: Integer; const Text: String): Boolean;
var Rect: TRectf;
var Color: TColor4f;
var Skin : TPHXSkinElement;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    Result:= True;
  end;

  if Pressed = Ident then
  begin
    Skin:= GetWidgetSkin(teButtonPressed);
  end else
  if Hover = Ident then
  begin
    Skin:= GetWidgetSkin(teButtonHover);
  end else
  begin
    Skin:= GetWidgetSkin(teButtonNormal);
  end;

  if Assigned(Skin) then
  begin
    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);

    Color:= Skin.Color;
  end else
  begin
    Color:= clrWhite;
  end;

  Color.Alpha:= FAlpha;
  Color:= clrWhite;

  Font.TextOut(Rect, Text, taCenter, Color);
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Edit(const X, Y, Width, Height: Integer; var Text: String): Boolean;
var Rect : TRectf;
var Color: TColor4f;
var Skin : TPHXSkinElement;
//var Cursor: TPHXSkinElement;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Focused = Ident then
  begin
    if KeyText <> #0 then
    begin
      Text:= Text + KeyText;

      Result:= True;

      KeyText:= #0;
    end;
    if KeyPressed = VK_BACKSPACE then
    begin
      Text:= Copy(Text,1, Length(Text) - 2 );

      Result:= True;

      KeyPressed:= VK_UNKNOWN;
    end;
  end;

  Skin:= GetWidgetSkin(teEditNormal);

  if Assigned(Skin) then
  begin
    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);

    Color:= Skin.Color;

    Rect:= Skin.GetClientRect(Rect);
  end else
  begin
    Color:= clrWhite;
  end;

  Color.Alpha:= FAlpha;

  // Draw text with blinking cursor
//  if Assigned(Cursor) and (Focused = Ident) and (Trunc(FTime * 2) mod 2 = 0) then
  if (Focused = Ident) and (Trunc(FTime * 2) mod 2 = 0) then
  begin
 //   Cursor.Draw(FSkin.Canvas, Vector3f(X + Font.TextWidth(Text), Y, 0.0), Vector2f(Cursor.Width, Height), FAlpha);
    Font.TextOut(Rect, Text + '|', taLeft, Color);
  end else
  begin
    Font.TextOut(Rect, Text      , taLeft, Color);
  end;
//  Font.DrawText(Rect, Text      , taLeft, Color);
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.CheckBox(const X, Y, Width, Height: Integer; const Text: String; var Checked: Boolean): Boolean;
var Rect : TRectf;
var Color: TColor4f;
var Skin : TPHXSkinElement;
var Size : TVector2i;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if (FPressed = Ident) then
  begin
    Checked:= not Checked;

    Result:= True;

    MousePressed:= False;
  end;

  if Checked then
  begin
    Skin:= GetWidgetSkin(teCheckBoxChecked);
  end else
  begin
    if FHover = FIdent then
    begin
      Skin:= GetWidgetSkin(teCheckBoxHover);
    end else
    begin
      Skin:= GetWidgetSkin(teCheckBoxNormal);
    end;
  end;

  if Assigned(Skin) then
  begin
    Size:= Skin.Size;

    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Size.X, Size.Y), FAlpha);

    Color:= Skin.Color;
    Color.Alpha:= FAlpha;
  end else
  begin
    Size.X:= 0;
    Size.Y:= 0;

    Color:= clrWhite;
  end;
  Rect.Left:= Rect.Left + Size.X + 2;

  Font.TextOut(Rect, Text, taLeft,  Color);
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Slider(const X, Y, Width, Height: Integer; const Min, Max: Single; var Value: Single): Boolean;
var Rect : TRectf;
var Track: TPHXSkinElement;
var Thumb: TPHXSkinElement;
var P    : Single;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Focused = Ident then
  begin

    if KeyPressed = VK_RIGHT then
    begin
      Value:= Value + (Max - Min) * 0.1;
      if Value > Max then Value:= Max;
      if Value < Min then Value:= Min;
      Result:= True;
    end;
    if KeyPressed = VK_LEFT then
    begin
      Value:= Value - (Max - Min) * 0.1;
      if Value > Max then Value:= Max;
      if Value < Min then Value:= Min;
      Result:= True;
    end;
    if KeyPressed = VK_HOME then
    begin
      Value:= Min;
      Result:= True;
    end;
    if KeyPressed = VK_END then
    begin
      Value:= Max;
      Result:= True;
    end;
  end;

  Track:= Skin.Parts[teSliderTrackNormal];
  Thumb:= Skin.Parts[teSliderThumbNormal];

  // Update the value when focused
  if Assigned(Thumb) and (Pressed = Ident) then
  begin
    Value:= Min + (MousePos.X - Position.X - X - (Thumb.Width * 0.5)) / (Width - Thumb.Width) * (Max - Min);

    if Value > Max then Value:= Max;
    if Value < Min then Value:= Min;

    Result:= True;
  end;

  if Assigned(Track) then
  begin
    Track.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);
  end;

  if Assigned(Thumb) then
  begin
    p:= Trunc( (Value - Min) / (Max - Min) *  (Width - (Thumb.Width)));

    Thumb.Draw(FSkin.Canvas, Vector3f(Rect.Left + P, Rect.Top, 0.0), Vector2f(Thumb.Width, Height), FAlpha);
  end;

end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.ProgressBar(const X, Y, Width, Height: Integer; const Progress: Single; const Direction: TProgressDirection = pdHorisontal): Boolean;
var Rect : TRectf;
var Frame: TPHXSkinElement;
var Thumb: TPHXSkinElement;
var P    : Single;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    Result:= True;
  end;

  Frame:= Skin.Parts[teProgressBarFrame];
  if Assigned(Frame) then
  begin
    Frame.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);
  end;

  case Direction of
    pdHorisontal:
    begin
      Thumb:= Skin.Parts[teProgressBarHorz];
      if Assigned(Thumb) then
      begin
        p:= Trunc( Progress * Width);

        Thumb.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(p, Height), FAlpha);
      end;
    end;
    pdVertical:
    begin
      Thumb:= Skin.Parts[teProgressBarVert];
      if Assigned(Thumb) then
      begin
        p:= Trunc( Progress * Height);

        Thumb.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, P), FAlpha);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSimpleGUI.Selector(const X, Y, Width, Height: Integer; const Values: TStrings; var Selected: Integer): Boolean;
var Rect : TRectf;
var Color: TColor4f;
var Skin : TPHXSkinElement;
begin
  Assert(FActive, SNotActive);

  Result:= False;

  Inc(FIdent);

  // Get the screen rectangle of this widget
  Rect:= GetWidgetRect(X, Y, Width, Height);

  FocusHandler(Ident, Rect);

  if Pressed = Ident then
  begin
    if MousePos.X < X + (Width div 2) then
    begin
      Dec(Selected);

      if Selected < 0 then Selected:= 0;
    end else
    begin
      Inc(Selected);

      if Selected >= Values.Count then Selected:= Values.Count-1;
    end;

    MousePressed:= False;

    Result:= True;
  end;

  if Focused = Ident then
  begin
    if KeyPressed = VK_LEFT then
    begin
      Dec(Selected);

      if Selected < 0 then Selected:= 0;

      KeyPressed:= VK_UNKNOWN;

      Result:= True;
    end;
    if KeyPressed = VK_RIGHT then
    begin
      Inc(Selected);

      if Selected >= Values.Count then Selected:= Values.Count-1;

      KeyPressed:= VK_UNKNOWN;

      Result:= True;
    end;
  end;
  Skin:= GetWidgetSkin(teEditNormal);

  if Assigned(Skin) then
  begin
    Skin.Draw(FSkin.Canvas, Vector3f(Rect.Left, Rect.Top, 0.0), Vector2f(Width, Height), FAlpha);

    Color:= Skin.Color;
  end else
  begin
    Color:= clrWhite;
    Color.Alpha:= FAlpha;
  end;

  if (Selected >=0) and (Selected < Values.Count) then
  begin
    Font.TextOut(Rect, Values[Selected], taCenter, Color);
  end;
end;








end.

