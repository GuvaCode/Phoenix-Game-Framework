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
unit phxSkin;
//< Skinning support for the gui engine

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxTypes,
  phxClasses,
  phxMath,
  phxDevice,
  phxGraphics,
  phxTexture,
  phxCanvas;


const

PHXSKIN_EXT = '.phxskn';
// Version
PHXSKIN_VERSION = 7;

type

// Forward declarations
TPHXSkin = class;

// List of controls
//------------------------------------------------------------------------------
TPHXThemedControl = (
  // Unknown control
  tcUnknown,
  // TPHXWindow
  tcWindow,
  // TPHXPanel
  tcPanel,
  // TPHXFrame
  tcFrame,
  // TPHXButton
  tcButton,
  // TPHXCheckBox
  tcCheckBox,
  // TPHXRadioButton
  tcRadioButton,
  // TPHXEdit
  tcEdit,
  // TPHXSlider
  tcSlider,
  // TPHXProgressbar,
  tcProgressBar,
  // TPHXScrollbar
  tcScrollbar,
  // TPHXListBox
  tcListBox
);

// List of parts for the controls
//------------------------------------------------------------------------------
TPHXThemedPart = (
  // Unknown part
  teUnknown,

  // TPHXWindow
  teWindowNormal,
  teWindowHover,
  teWindowDisabled,

  //TPHXPanel
  tePanelNormal,
  tePanelDisabled,

  // TPHXFrame
  teFrameNormal,
  teFrameDisabled,

  // TPHXButton
  teButtonNormal,
  teButtonHover,
  teButtonDisabled,
  teButtonPressed,
  teButtonFocused,

  // TPHXCheckBox
  teCheckBoxNormal,
  teCheckBoxHover,
  teCheckBoxChecked,
  teCheckBoxDisabled,

  // TPHXRadioButton
  teRadioButtonNormal,
  teRadioButtonHover,
  teRadioButtonChecked,
  teRadioButtonDisabled,

  // TPHXEdit
  teEditNormal,
  teEditHover,
  teEditDisabled,
  teEditCursor,
  teEditSelection,

  // TPHXProgressbar,
  teProgressBarFrame,
  teProgressBarHorz,
  teProgressBarVert,

  // TPHXSlider
  teSliderTrackNormal,
  teSliderTrackDisabled,
  teSliderThumbNormal,
  teSliderThumbHover,
  teSliderThumbDisabled,

  // TPHXScrollbar
  teScrollbarTrackNormal,
  teScrollbarTrackDisabled,

  teScrollbarThumbNormal,
  teScrollbarThumbDisabled,
  teScrollbarThumbHover,

  teScrollbarButtonIncNormal,
  teScrollbarButtonIncHover,
  teScrollbarButtonIncDisabled,
  teScrollbarButtonIncPressed,

  teScrollbarButtonDecNormal,
  teScrollbarButtonDecHover,
  teScrollbarButtonDecDisabled,
  teScrollbarButtonDecPressed,

  // TPHXListBox
  teListBoxNormal,
  teListBoxDisabled,
  teListBoxItemNormal,
  teListBoxItemSelected
);


{$REGION 'TPHXSkinElement'}

(*
  // Texture coordinates for the element
  TextureBounds
  // Size of the fixed margins that doesnt change size when resizing the control
  FixedMargins
  // Margins that are drawn outside the control for shadows and such
  OuterMargins
*)

// Contains the texture coordinates and properties for a element describing the
// apperance for a control in a given state
//------------------------------------------------------------------------------
TPHXSkinElement = class
  private
    // Owning skin
    FSkin: TPHXSkin;
    // Name of the skin element
    FName: String;
    // This is the theme part
    FPart: TPHXThemedPart;

    // element color
    FColor: TColor4f;
    // The texture bounds
    FBounds: TRecti;
    // Margins that doesnt change size when resizing the control
    FMargins: TRecti;
    // Margins that are drawn outside the control for shadows and such
    FShadow: TRecti;

    // Margins for the control text
    FTextPadding: TRecti;
    // Color for the control text
    FTextColor : TColor4f;

    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetSize: TVector2i;
    procedure SetName(const Value: String);
  protected
    // Number of required primitives
    PrimitiveCount: Integer;

    Vertices: array[0..15] of TPHXVertex;
    Indicies: array[0..53] of TPHXIndex;

    // Load the element from a reader
    procedure LoadElement(Reader: TPHXReader);
    // Save the element to a writer
    procedure SaveElement(Writer: TPHXWriter);
  public
    constructor Create(ASkin: TPHXSkin); virtual;
    destructor Destroy; override;

    // Intialize the skin, must be called before rendering the skin
    procedure Initialize(Texture: TPHXTexture);

    // Load from file
    procedure LoadFromFile(const FileName: String);
    // Load from stream
    procedure LoadFromStream(Stream: TStream);
    // Save the image to a file
    procedure SaveToFile(const FileName: String);
    // Save the image to a stream
    procedure SaveToStream(Stream: TStream);

    // Render the skin element
    procedure Draw(Canvas: TPHXCanvas; const Position: TVector3f; const Size: TVector2f; const Alpha: Single); overload;
    // Render Draw skin element
    procedure Draw(Canvas: TPHXCanvas; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single); overload;

    // Get the client rectangle for a state, the input bounds are shrinked with the area Margins
    function GetClientRect(const Bounds: TRectf): TRectf;

    // The owning skin
    property Skin: TPHXSkin read FSkin;
    // Name of the skin element
    property Name: String read FName write SetName;
    // The part this element is
    property Part: TPHXThemedPart read FPart write FPart;
    // Calculate the width of the skin element
    property Width: Integer read GetWidth;
    // Calculate the height of the skin element
    property Height: Integer read GetHeight;
    // Calculate the size of the skin element
    property Size: TVector2i read GetSize;
    // Color of the element
    property Color: TColor4f read FColor write FColor;
    // The texture coordinates
    property Bounds: TRecti read FBounds write FBounds;
    // Size of the fixed margins that doesnt change size when resizing the control
    property Margins: TRecti read FMargins write FMargins;
    // Margins that are drawn outside the control for shadows and such
    property Shadow: TRecti read FShadow write FShadow;

    // Padding for the control text
    property TextPadding: TRecti read FTextPadding write FTextPadding;
    // Color for the control text
    property TextColor: TColor4f read FTextColor write FTextColor;
  end;

{$ENDREGION}

{$REGION 'TPHXSkinElements'}

PSkinElementList = ^TSkinElementList;
TSkinElementList = array[0..$00FFFFFF] of TPHXSkinElement;

//------------------------------------------------------------------------------
TPHXSkinElementList = class
  private
    FOwner: TPHXSkin;
    FList : TList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TPHXSkinElement;
    function GetList: PSkinElementList;
  public
    constructor Create(AOwner: TPHXSkin);
    destructor Destroy; override;

    // Free and delete all states
    procedure Clear;
    // Add a new element to the list
    function Add: TPHXSkinElement;
    // Free and delete a element at and index
    procedure Delete(Index: Integer);

    function IndexOf(const Element: TPHXSkinElement): Integer;

    // Find a skin element by its name
    function Find(const Name: String): TPHXSkinElement; overload;
    // Find a skin element by its name
    function Find(const Name: String; out State: TPHXSkinElement): Boolean; overload;
    // Find a skin element by a theme element
//    function Find(const Name: TPHXThemedPart): TPHXSkinElement; overload;


    property Owner: TPHXSkin read FOwner;
    // Number of skin elements in the list
    property Count: Integer read GetCount;
    // Pointer to the internal list
    property List: PSkinElementList read GetList;
    // Gets the items in the list
    property Items[Index: Integer]: TPHXSkinElement read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXSkin'}

// The skin file header.
//------------------------------------------------------------------------------
TPHXSkinHeader = record
  // The id of the image file, should always be PHXSKIN.
  Ident: array[1..7] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// Contains a list of skin elements
//------------------------------------------------------------------------------
TPHXSkin = class
  private
    FDevice: TPHXDevice;
    FCanvas: TPHXCanvas;

    // Name of the skin
    FName    : String;
    FVersion: String;
    FAuthor : String;
    FComment: String;

    // Width of the texture
    FWidth: Integer;
    // Height of the texture
    FHeight: Integer;
    // List of skin element
    FElements: TPHXSkinElementList;
    // The texture of the skin
    FTexture: TPHXTexture;

    // Lookup map for the themed parts
    FParts: array[TPHXThemedPart] of TPHXSkinElement;
    function GetPart(const Index: TPHXThemedPart): TPHXSkinElement;
  protected
    // Load the skin from a reader
    procedure LoadSkin(Reader: TPHXReader); overload;
    // Save the skin to a writer
    procedure SaveSkin(Writer: TPHXWriter); overload;
  public
    // Default constructor
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
    // Constructor used by the editors
    constructor CreateEx(ATexture: TPHXTexture);
    // Default destructor
    destructor Destroy; override;

    // Intialize the skin, must be called before rendering the skin
    procedure Initialize;

    // Load a skin from a file.
    procedure LoadSkin(const FileName: String); overload;

    // Load the skin from from a file
    procedure LoadFromFile(FileName: String);
    // Save the skin to from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the skin to a file
    procedure SaveToFile(FileName: String);
    // Load the skin from from a stream
    procedure SaveToStream(Stream: TStream);

    // Draw a control part
    procedure Draw(Element: TPHXThemedPart; const Position: TVector3f; const Size: TVector2f; const Alpha: Single); overload;
    // Render Draw skin element
    procedure Draw(Element: TPHXThemedPart; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single); overload;


    // Find a skin element by name
    function FindElement(const Name: String): TPHXSkinElement; overload;
    // Find a skin element by name
    function FindElement(const Name: String; Default: TPHXSkinElement): TPHXSkinElement; overload;

    function ElementAt(const X, Y: Integer): Integer;

    // The owning device
    property Device: TPHXDevice read FDevice;
    // The owning canvas
    property Canvas: TPHXCanvas read FCanvas;
    // Name of the skin
    property Name  : String  read FName write FName;
    // Version of the skin
    property Version: String read FVersion write FVersion;
    // Version of the skin
    property Author: String read FAuthor write FAuthor;
    // Skin Comment
    property Comment: String read FComment write FComment;
    // Width of the texture
    property Width : Integer read FWidth write FWidth;
    // Height of the texture
    property Height: Integer read FHeight write FHeight;
    // Returns an skin from the list
    property Elements: TPHXSkinElementList read FElements;
    // The texture for the skin
    property Texture : TPHXTexture  read FTexture;
    // Return a element from the part name
    property Parts[const Index: TPHXThemedPart]: TPHXSkinElement read GetPart;
 end;

{$ENDREGION}


{$REGION 'TPHXSkinTransition'}

// Class for managing transitions between skin states
//------------------------------------------------------------------------------
TPHXSkinTransition = class
  private
    FElement    : TPHXSkinElement;
    FPrevious   : TPHXSkinElement;
    FDuration   : Single;
    FDurationInv: Single;
    FTime       : Single;
    FPosition   : Single;
    FActive     : Boolean;

    procedure SetElement(const Value: TPHXSkinElement);
    procedure SetDuration(const Value: Single);
  public
    // Create a new transition
    constructor Create; overload;
    // Create a new transition with a active element
    constructor Create(AElement: TPHXSkinElement); overload;
    // Destroy the transition
    destructor Destroy; override;

    // Update the skin transition
    procedure Update(const DeltaTime: Double);

    // Render the skin transition
    procedure Draw(Canvas: TPHXCanvas; const Position: TVector3f; const Size: TVector2f; const Alpha: Single); overload;
    // Render Draw skin transition
    //procedure Draw(Canvas: TPHXCanvas; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single); overload;

    // The transition duration in seconds
    property Duration: Single read FDuration write SetDuration;
    // The current skin element
    property Element: TPHXSkinElement read FElement write SetElement;
    // The previous skin element, null if not transitioning
    property Previous: TPHXSkinElement read FPrevious;
    // The transition is active
    property Active: Boolean read FActive;
    // The current transition time
    property Time: Single read FTime;
    // The current transition position
    property Position: Single read FPosition;
  end;

{$ENDREGION}
    (*

TPHXTextTheme = class
  Color: TColor32;
  Offset: TVector2i;
end;

TPHXSkinTheme = class
  Element: TPHXSkinElement;
end;

TPHXStyle = class
  private
    FName: String;
  public
    property Name: String read FName write FName;
  end;
TPHXStyleClass = class of TPHXStyle;

TPHXWindowStyle = class(TPHXStyle)
  private
  public
  end;

TPHXButtonStyle = class(TPHXStyle)
  private
    FText: TPHXTextTheme;
    FNormal: TPHXSkinTheme;
    FHover: TPHXSkinTheme;
    FPressed: TPHXSkinTheme;
    FDisabled: TPHXSkinTheme;
  public
    property Text: TPHXTextTheme read FText write FText;
    property Normal: TPHXSkinTheme read FNormal write FNormal;
    property Pressed: TPHXSkinTheme read FPressed write FPressed;
    property Hover: TPHXSkinTheme read FHover write FHover;
    property Disabled: TPHXSkinTheme read FDisabled write FDisabled;
  end;

TPHXCheckboxStyle = class(TPHXStyle)
  private
    FNormal: TPHXSkinTheme;
    FDisabled: TPHXSkinTheme;
    FText: TPHXTextTheme;
  public
    property Text: TPHXTextTheme read FText write FText;
    property Normal: TPHXSkinTheme read FNormal write FNormal;
    property Checked: TPHXSkinTheme read FNormal write FNormal;
    property Disabled: TPHXSkinTheme read FDisabled write FDisabled;
  end;

procedure RegisterStyle(const Name: String; const Style: TPHXStyleClass);
    *)


// Returns the part before the dot in the element name
function GetControlName(const Name: String): String;
// Returns the part after the dot in the skin name
function GetPartName(const Name: String): String;

// Get the name for a TPHXThemedControl
function ControlToString(const Value: TPHXThemedControl): String;
// Converts a themed part to string
function PartToString(const Value: TPHXThemedPart ): String;

// Converts a string to a themed part
function StringToPart(const Value: String): TPHXThemedPart;



// Fill the list with the default element names
//procedure ListControls(const Lines: TStrings);
// Fill the list with the default state names for the given control
//procedure ListElements(const Lines: TStrings; const Control: String); overload;





implementation

//------------------------------------------------------------------------------
const ControlNames: array[TPHXThemedControl] of String = (
  // tcUnknown,
  '',
  // tcWindow,
  'TPHXWindow',
  // tcPanel
  'TPHXPanel',
  // tcFrame
  'TPHXFrame',
  // tcButton
  'TPHXButton',
  // tcCheckBox
  'TPHXCheckBox',
  // tcRadioButton
  'TPHXRadioButton',
  // tcEdit
  'TPHXEdit',
  // tcSlider
  'TPHXSlider',
  // tcProgressBar
  'TPHXProgressbar',
  // tcScrollbar
  'TPHXScrollbar',
  // tcListBox
  'TPHXListBox'
);

//------------------------------------------------------------------------------
const PartNames: array[TPHXThemedPart] of String = (
  // teUnknown:
  '',
  // TPHXWindow
  { teWindowNormal  } 'TPHXWindow.Normal',
  { teWindowHover   } 'TPHXWindow.Hover',
  { teWindowDisabled} 'TPHXWindow.Disabled',

  // TPHXPanel
  { tePanelNormal   }'TPHXPanel.Normal',
  { tePanelDisabled }'TPHXPanel.Disabled',

  // TPHXFrame
  { teFrameNormal   }'TPHXFrame.Normal',
  { teFrameDisabled }'TPHXFrame.Disabled',

  // TPHXButton
  { teButtonNormal  }'TPHXButton.Normal',
  { teButtonHover   }'TPHXButton.Hover',
  { teButtonDisabled}'TPHXButton.Disabled',
  { teButtonPressed }'TPHXButton.Pressed',
  { teButtonFocused }'TPHXButton.Focused',

  // TPHXCheckBox
  { teCheckBoxNormal  }'TPHXCheckBox.Normal',
  { teCheckBoxHover   }'TPHXCheckBox.Hover',
  { teCheckBoxChecked }'TPHXCheckBox.Checked',
  { teCheckBoxDisabled}'TPHXCheckBox.Disabled',

  // TPHXRadioButton
  { teRadioButtonNormal   }'TPHXRadioButton.Normal',
  { teRadioButtonHover    }'TPHXRadioButton.Hover',
  { teRadioButtonChecked  }'TPHXRadioButton.Checked',
  { teRadioButtonDisabled }'TPHXRadioButton.Disabled',

  // TPHXEdit
  { teEditNormal    }'TPHXEdit.Normal',
  { teEditHover     }'TPHXEdit.Hover',
  { teEditDisabled  }'TPHXEdit.Disabled',
  { teEditCursor    }'TPHXEdit.Cursor',
  { teEditSelection }'TPHXEdit.Selection',

  // TPHXProgressbar
  { teProgressBarFrame}
  'TPHXProgressbar.Frame',
  { teProgressBarHorz }
  'TPHXProgressbar.Horz',
  { teProgressBarVert }
  'TPHXProgressbar.Vert',

  // TPHXSlider
  'TPHXSlider.Track.Normal',
  'TPHXSlider.Track.Disabled',
  'TPHXSlider.Thumb.Normal',
  'TPHXSlider.Thumb.Hover',
  'TPHXSlider.Thumb.Disabled',

  // TPHXScrollbar
  { teScrollbarTrackNormal      }'TPHXScrollbar.Track.Normal',
  { teScrollbarTrackDisabled    }'TPHXScrollbar.Track.Disabled',

  { teScrollbarThumbNormal      }'TPHXScrollbar.Thumb.Normal',
  { teScrollbarThumbDisabled    }'TPHXScrollbar.Thumb.Disabled',
  { teScrollbarThumbHover       }'TPHXScrollbar.Thumb.Hover',

  { teScrollbarButtonIncNormal  }'TPHXScrollbar.ButtonInc.Normal',
  { teScrollbarButtonIncHover   }'TPHXScrollbar.ButtonInc.Hover',
  { teScrollbarButtonIncDisabled}'TPHXScrollbar.ButtonInc.Disabled',
  { teScrollbarButtonIncPressed }'TPHXScrollbar.ButtonInc.Pressed',

  { teScrollbarButtonDecNormal  }'TPHXScrollbar.ButtonDec.Normal',
  { teScrollbarButtonDecHover   }'TPHXScrollbar.ButtonDec.Hover',
  { teScrollbarButtonDecDisabled}'TPHXScrollbar.ButtonDec.Disabled',
  { teScrollbarButtonDecPressed }'TPHXScrollbar.ButtonDec.Pressed',

  // TPHXListBox
  { teListBoxNormal       }
  'TPHXListBox.Normal',
  { teListBoxDisabled     }
  'TPHXListBox.Disabled',
  { teListBoxItemNormal   }
  'TPHXListBox.Item.Normal',
  { teListBoxItemSelected }
  'TPHXListBox.Item.Selected'
);

// Returns the part before the dot in the element name
//------------------------------------------------------------------------------
function GetControlName(const Name: String): String;
var Index: Integer;
begin
  Index:= Pos('.', Name);

  Result:= Copy(Name, 1 , Index-1);
end;

// Returns the part after the dot in the skin name
//------------------------------------------------------------------------------
function GetPartName(const Name: String): String;
var Index: Integer;
begin
  Index:= Pos('.', Name);

  Result:= Copy(Name, Index+1, MaxInt);
end;

//------------------------------------------------------------------------------
function ControlToString(const Value: TPHXThemedControl): String;
begin
  Result:= ControlNames[Value];
end;

//------------------------------------------------------------------------------
function PartToString(const Value: TPHXThemedPart ): String;
begin
  Result:= PartNames[Value];
end;

//------------------------------------------------------------------------------
function StringToPart(const Value: String): TPHXThemedPart;
var Part : TPHXThemedPart;
begin
  // Clear the part map
  for Part:= Low(TPHXThemedPart) to High(TPHXThemedPart) do
  begin
    if SameText(Value, PartNames[Part]) then
    begin
      Result:= Part;

      Exit;
    end;
  end;
  Result:= teUnknown;
end;





//------------------------------------------------------------------------------
procedure ListControls(const Lines: TStrings);
var Control: TPHXThemedControl;
begin
  for Control := Low(TPHXThemedControl) to High(TPHXThemedControl) do
  begin
    Lines.Add( ControlToString(Control) );
  end;
end;

//------------------------------------------------------------------------------
procedure ListParts(const Lines: TStrings; const Control: String);
var Part: TPHXThemedPart;
var Name: String;
begin
  Lines.Clear;

  for Part := Low(TPHXThemedPart) to High(TPHXThemedPart) do
  begin
    Name:= PartToString(Part);

    if SameText(Control, GetControlName(Name)) then
    begin
      Lines.Add(Name);
    end;
  end;
end;



{$REGION 'TPHXSkinElement'}

// TPHXSkinElement
//==============================================================================
constructor TPHXSkinElement.Create(ASkin: TPHXSkin);
begin
  FSkin := ASkin;
  Name  := 'Unnamned';
  FColor:= clrWhite;

  FBounds.Left  := 0;
  FBounds.Top   := 0;
  FBounds.Right := 0;
  FBounds.Bottom:= 0;

  FMargins.Left  := 0;
  FMargins.Top   := 0;
  FMargins.Right := 0;
  FMargins.Bottom:= 0;

  FShadow.Left  := 0;
  FShadow.Top   := 0;
  FShadow.Right := 0;
  FShadow.Bottom:= 0;

  FTextPadding.Left  := 0;
  FTextPadding.Top   := 0;
  FTextPadding.Right := 0;
  FTextPadding.Bottom:= 0;
  FTextColor         := clrWhite;
end;

//------------------------------------------------------------------------------
destructor TPHXSkinElement.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.LoadElement(Reader: TPHXReader);
begin
  FName   :=                Reader.ReadString;
  FPart   := TPHXThemedPart(Reader.ReadCardinal);
  FColor  :=                Reader.ReadColor;
  FBounds :=                Reader.ReadRecti;
  FMargins:=                Reader.ReadRecti;
  FShadow :=                Reader.ReadRecti;

  FTextPadding  := Reader.ReadRecti;
  FTextColor    := Reader.ReadColor;

  FPart:= StringToPart(FName);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.SaveElement(Writer: TPHXWriter);
begin
  Writer.WriteString(FName);
  Writer.WriteCardinal( Ord(FPart) );
  Writer.WriteColor(FColor);
  Writer.WriteRecti(FBounds);
  Writer.WriteRecti(FMargins);
  Writer.WriteRecti(FShadow);

  Writer.WriteRecti(FTextPadding);
  Writer.WriteColor(FTextColor);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.LoadFromStream(Stream: TStream);
var Reader: TPHXReader;
begin
  Reader:= TPHXReader.Create(Stream);
  try
    LoadElement(Reader);
  finally
    Reader.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.SaveToStream(Stream: TStream);
var Writer: TPHXWriter;
begin
  Writer:= TPHXWriter.Create(Stream);
  try
    SaveElement(Writer);
  finally
    Writer.Free;
  end;
end;
//                  Width
//       +---------------------------------------+
//   B.L   M.L                               M.R |
//  +----+----+-----------------------------+----+
//  |    |    |                             |    |
//  +----+----+-----------------------------+----+
//  |    |    |                             |    |
//  |    |    |                             |    |
//  |    |    |                             |    |
//  |    |    |                             |    |
//  |    |    |                             |    |
//  |    |    |                             |    |
//  +----+----+-----------------------------+----+
//  |    |    |                             |    |
//  +----+----+-----------------------------+----+


//------------------------------------------------------------------------------
procedure TPHXSkinElement.Initialize(Texture: TPHXTexture);
var InvWidth : Single;
var InvHeight: Single;
var Margins  : TRecti;
var Coords   : array[0..3] of TVector2f;
var I,X,Y    : Integer;
begin
  // Test if the width or the height of the bounds is zero,
  if ((Bounds.Right - Bounds.Left) = 0) or ((Bounds.Bottom - Bounds.Top) = 0) then
  begin
    PrimitiveCount:= 0;

    Exit;
  end;

  // Avoid division by zero
  if (Texture.Width = 0) or (Texture.Height = 0) then
  begin
    InvWidth := 0.0;
    InvHeight:= 0.0;
  end else
   begin
    InvWidth := 1.0 / Texture.Width;
    InvHeight:= 1.0 / Texture.Height;
  end;

  // Calculate the total margin size
  Margins.Left  := FMargins.Left   + FShadow.Left;
  Margins.Right := FMargins.Right  + FShadow.Right;
  Margins.Top   := FMargins.Top    + FShadow.Top;
  Margins.Bottom:= FMargins.Bottom + FShadow.Bottom;

  // Test if we need a single rectangle or nine
  if (Margins.Left = 0) and (Margins.Right = 0) and (Margins.Top = 0) and (Margins.Bottom = 0) then
  begin
    PrimitiveCount:= 1;

    Coords[0].X:= (Bounds.Left  - FShadow.Left  ) * InvWidth;
    Coords[1].X:= (Bounds.Right + FShadow.Right ) * InvWidth;

    Coords[0].Y:= (Bounds.Top    - FShadow.Top    ) * InvHeight;
    Coords[1].Y:= (Bounds.Bottom + FShadow.Bottom ) * InvHeight;

    I:=0;
    for Y:= 0 to 1 do
    begin
      for X:= 0 to 1 do
      begin
        Vertices[I].Color     := Color;
        Vertices[I].Normal    := Vector3f_AxisZ;
        Vertices[I].Position.X:= 0;
        Vertices[I].Position.Y:= 0;
        Vertices[I].Position.Z:= 0;
        Vertices[I].TexCoord.X:= Coords[X].X;
        Vertices[I].TexCoord.Y:= Coords[Y].Y;
        Inc(I);
      end;
    end;

    // Triangle #1
    Indicies[0]:= 0;
    Indicies[1]:= 1;
    Indicies[2]:= 2;
    // Triangle #2
    Indicies[3]:= 1;
    Indicies[4]:= 2;
    Indicies[5]:= 3;
  end else
  begin
    PrimitiveCount:= 9;

    Coords[0].X:= (Bounds.Left  - FShadow.Left  ) * InvWidth;
    Coords[1].X:= (Bounds.Left  + FMargins.Left ) * InvWidth;
    Coords[2].X:= (Bounds.Right - FMargins.Right) * InvWidth;
    Coords[3].X:= (Bounds.Right + FShadow.Right ) * InvWidth;

    Coords[0].Y:= (Bounds.Top    - FShadow.Top    ) * InvHeight;
    Coords[1].Y:= (Bounds.Top    + FMargins.Top   ) * InvHeight;
    Coords[2].Y:= (Bounds.Bottom - FMargins.Bottom) * InvHeight;
    Coords[3].Y:= (Bounds.Bottom + FShadow.Bottom ) * InvHeight;

    I:=0;
    for Y:= 0 to 3 do
    begin
      for X:= 0 to 3 do
      begin
        Vertices[I].Color     := Color;
        Vertices[I].Normal    := Vector3f_AxisZ;
        Vertices[I].Position.X:= 0;
        Vertices[I].Position.Y:= 0;
        Vertices[I].Position.Z:= 0;
        Vertices[I].TexCoord.X:= Coords[X].X;
        Vertices[I].TexCoord.Y:= Coords[Y].Y;
        Inc(I);
      end;
    end;

    I:=0;
    for Y:= 0 to 2 do
    begin
      for X:= 0 to 2 do
      begin
        Indicies[I + 0]:= X + (Y * 4) + 0;
        Indicies[I + 1]:= X + (Y * 4) + 1;
        Indicies[I + 2]:= X + (Y * 4) + 4;
        // Triangle #2
        Indicies[I + 3]:= X + (Y * 4) + 1;
        Indicies[I + 4]:= X + (Y * 4) + 5;
        Indicies[I + 5]:= X + (Y * 4) + 4;

         Inc(I, 6);
       end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.Draw(Canvas: TPHXCanvas; const Position: TVector3f; const Size: TVector2f; const Alpha: Single);
var V: array[0..3] of TVector2f;
var X,Y,I: Integer;
begin
  // Empty rectangle or texture
  if (Size.X = 0) or (Size.Y = 0) then Exit;

  if PrimitiveCount = 1 then
  begin
    V[0].X:= Position.X           - FShadow.Left;
    V[1].X:= Position.X + Size.X  + FShadow.Right;

    V[0].Y:= Position.Y           - FShadow.Top;
    V[1].Y:= Position.Y + Size.Y  + FShadow.Bottom;

    I:=0;
    for Y:= 0 to 1 do
    begin
      for X:= 0 to 1 do
      begin
        Vertices[I].Color.Alpha:= Alpha;
        Vertices[I].Position.X:= V[X].X;
        Vertices[I].Position.Y:= V[Y].Y;
        Vertices[I].Position.Z:= Position.Z;
        Inc(I);
      end;
    end;

    Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, Skin.Texture );
  end else
  begin
    V[0].X:= Position.X           - FShadow.Left;
    V[1].X:= Position.X           + FMargins.Left;
    V[2].X:= Position.X + Size.X  - FMargins.Right;
    V[3].X:= Position.X + Size.X  + FShadow.Right;

    V[0].Y:= Position.Y           - FShadow.Top;
    V[1].Y:= Position.Y           + FMargins.Top;
    V[2].Y:= Position.Y + Size.Y  - FMargins.Bottom;
    V[3].Y:= Position.Y + Size.Y  + FShadow.Bottom;

    I:=0;
    for Y:= 0 to 3 do
    begin
      for X:= 0 to 3 do
      begin
        Vertices[I].Color.Alpha:= Alpha;

        Vertices[I].Position.X:= V[X].X;
        Vertices[I].Position.Y:= V[Y].Y;
        Vertices[I].Position.Z:= Position.Z;
        Inc(I);
      end;
    end;

    Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 16, 54, Skin.Texture);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.Draw(Canvas: TPHXCanvas; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single);
var V: array[0..3] of TVector2f;
var Size2: TVector2f;
var X,Y,I: Integer;
begin
  // Empty rectangle or texture
  if (Size.X = 0) or (Size.Y = 0) then Exit;

  Size2.X:= Size.X * 0.5;
  Size2.Y:= Size.Y * 0.5;

  if PrimitiveCount = 1 then
  begin
    V[0].X:=         - FShadow.Left   - Size2.X;
    V[1].X:= Size.X  + FShadow.Right  - Size2.X;

    V[0].Y:=         - FShadow.Top    - Size2.Y;
    V[1].Y:= Size.Y  + FShadow.Bottom - Size2.Y;

    I:=0;
    for Y:= 0 to 1 do
    begin
      for X:= 0 to 1 do
      begin
        Vertices[I].Color.Alpha:= Alpha;
        Vertices[I].Position.X:= V[X].X;
        Vertices[I].Position.Y:= V[Y].Y;
        Vertices[I].Position.Z:= 0;

        Vertices[I].Position:= Matrix_Transform(Transform, Vertices[I].Position);

        Inc(I);
      end;
    end;

    Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, Skin.Texture );
  end else
  begin
    V[0].X:=         - FShadow .Left  - Size2.X;
    V[1].X:=         + FMargins.Left  - Size2.X;
    V[2].X:= Size.X  - FMargins.Right - Size2.X;
    V[3].X:= Size.X  + FShadow .Right - Size2.X;

    V[0].Y:=         - FShadow .Top    - Size2.Y;
    V[1].Y:=         + FMargins.Top    - Size2.Y;
    V[2].Y:= Size.Y  - FMargins.Bottom - Size2.Y;
    V[3].Y:= Size.Y  + FShadow .Bottom - Size2.Y;

    I:=0;
    for Y:= 0 to 3 do
    begin
      for X:= 0 to 3 do
      begin
        Vertices[I].Color.Alpha:= Alpha;

        Vertices[I].Position.X:= V[X].X;
        Vertices[I].Position.Y:= V[Y].Y;
        Vertices[I].Position.Z:= 0;

        Vertices[I].Position:= Matrix_Transform(Transform, Vertices[I].Position);
        Inc(I);
      end;
    end;

    Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 16, 54, Skin.Texture);
  end;
end;




//------------------------------------------------------------------------------
function TPHXSkinElement.GetClientRect(const Bounds: TRectf): TRectf;
begin
  Result.Left  := Bounds.Left   + Margins.Left;
  Result.Right := Bounds.Right  - Margins.Right;
  Result.Top   := Bounds.Top    + Margins.Top;
  Result.Bottom:= Bounds.Bottom - Margins.Bottom;
end;

//------------------------------------------------------------------------------
function TPHXSkinElement.GetWidth: Integer;
begin
  Result:= Bounds.Right - Bounds.Left;
end;

//------------------------------------------------------------------------------
function TPHXSkinElement.GetHeight: Integer;
begin
  Result:= Bounds.Bottom - Bounds.Top;
end;

//------------------------------------------------------------------------------
function TPHXSkinElement.GetSize: TVector2i;
begin
  Result.X:= Bounds.Right  - Bounds.Left;
  Result.Y:= Bounds.Bottom - Bounds.Top;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElement.SetName(const Value: String);
begin
  FName := Value;
end;

{$ENDREGION}

{$REGION 'TPHXSkinElementList'}

// TPHXSkinElementList
//==============================================================================
constructor TPHXSkinElementList.Create(AOwner: TPHXSkin);
begin
  FOwner:= AOwner;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXSkinElementList.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElementList.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    TPHXSkinElement(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.Add: TPHXSkinElement;
begin
  Result:= TPHXSkinElement.Create(Owner);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElementList.Delete(Index: Integer);
var Item: TPHXSkinElement;
begin
  Item:= TPHXSkinElement(FList.List[Index]);

  FList.Delete(Index);

  Item.Free;
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.IndexOf(const Element: TPHXSkinElement): Integer;
begin
  Result:= FList.IndexOf(Element);
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.Find(const Name: String): TPHXSkinElement;
var Index: Integer;
var Item: TPHXSkinElement;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Item:= TPHXSkinElement(FList.List[Index]);

    if SameText(Item.Name, Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.Find(const Name: String; out State: TPHXSkinElement): Boolean;
var Index: Integer;
var Item: TPHXSkinElement;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Item:= TPHXSkinElement(FList.List[Index]);

    if SameText(Item.Name, Name) then
    begin
      State := Item;
      Result:= True;
      Exit;
    end;
  end;
  Result:= False;
end;
         {
//------------------------------------------------------------------------------
function TPHXSkinElementList.Find(const Name: TPHXThemedPart): TPHXSkinElement;
begin
  Result:= Find( TPHXSkinFactory.GetElementName(Name) );
end;    }

//------------------------------------------------------------------------------
function TPHXSkinElementList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.GetList: PSkinElementList;
begin
  Result:= PSkinElementList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXSkinElementList.GetItem(Index: Integer): TPHXSkinElement;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXSkinElement(FList.List[Index]);
end;

{$ENDREGION}

{$REGION 'TPHXSkin'}

const DEFAULT_SKIN_NAME = 'Skin';

// TPHXSkin
//==============================================================================
constructor TPHXSkin.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
begin
  FElements:= TPHXSkinElementList.Create(Self);
  FDevice  := ADevice;
  FTexture := ADevice.CreateTexture;
  FCanvas  := ACanvas;
  FName    := DEFAULT_SKIN_NAME;
end;

//------------------------------------------------------------------------------
constructor TPHXSkin.CreateEx(ATexture: TPHXTexture);
begin
  FElements:= TPHXSkinElementList.Create(Self);
  FDevice  := nil;
  FTexture := ATexture;
  FCanvas  := nil;
  FName    := DEFAULT_SKIN_NAME;
end;

//------------------------------------------------------------------------------
destructor TPHXSkin.Destroy;
begin
  FTexture.Free;
  FElements.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.LoadSkin(Reader: TPHXReader);
var Count  : Integer;
var Index  : Integer;
var Current: TPHXSkinElement;
begin
  // The name of the skin.
  FName   := Reader.ReadString;
  FVersion:= Reader.ReadString;
  FAuthor := Reader.ReadString;
  FComment:= Reader.ReadString;

  FWidth := Reader.ReadInteger;
  FHeight:= Reader.ReadInteger;
  Count  := Reader.ReadInteger;

  FElements.Clear;
  // Write all skins
  for Index:=0 to Count - 1 do
  begin
    Current:= FElements.Add;

    Current.LoadElement(Reader);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.SaveSkin(Writer: TPHXWriter);
var Index  : Integer;
var Current: TPHXSkinElement;
begin
  Writer.WriteString(FName);
  Writer.WriteString(FVersion);
  Writer.WriteString(FAuthor);
  Writer.WriteString(FComment);

  Writer.WriteInteger(FWidth);
  Writer.WriteInteger(Height);
  Writer.WriteInteger(FElements.Count);

  // Write all skins
  for Index:=0 to FElements.Count - 1 do
  begin
    Current:= TPHXSkinElement( FElements.List^[Index] );

    Current.SaveElement(Writer);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.LoadSkin(const FileName: String);
begin
  LoadFromFile(FileName);
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.LoadFromFile(FileName: String);
var Stream: TStream;
 begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.SaveToFile(FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.LoadFromStream(Stream: TStream);
var Header: TPHXSkinHeader;
var Reader: TPHXReader;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  if (Header.Ident <> 'PHXSKIN') then
  begin
    raise Exception.Create('Not a valid Phoenix Skin.');
  end;
  if (Header.Version <> PHXSKIN_VERSION) then
  begin
    raise Exception.CreateFmt('Skin version missmatch [File: %d Code: %d].', [Header.Version, PHXSKIN_VERSION]);
  end;

  Reader:= TPHXReader.Create(Stream);
  try
    LoadSkin(Reader);
  finally
    Reader.Free;
  end;

  FTexture.LoadFromStream(Stream);

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.SaveToStream(Stream: TStream);
var Header: TPHXSkinHeader;
var Writer: TPHXWriter;
begin
  Header.Ident  := 'PHXSKIN';
  Header.Version:= PHXSKIN_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Writer:= TPHXWriter.Create(Stream);
  try
    SaveSkin(Writer);
  finally
    Writer.Free;
  end;

  FTexture.SaveToStream(Stream);
end;


//------------------------------------------------------------------------------
procedure TPHXSkin.Initialize;
var Index  : Integer;
var Part   : TPHXThemedPart;
var Element: TPHXSkinElement;
begin
  // Clear the part map
  for Part := Low(TPHXThemedPart) to High(TPHXThemedPart) do
  begin
    FParts[Part]:= nil;
  end;

  for Index := 0 to FElements.Count - 1 do
  begin
    Element:= Elements[Index];
    Element.Initialize(FTexture);

    FParts[Element.Part]:= Element;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.Draw(Element: TPHXThemedPart; const Position: TVector3f; const Size: TVector2f; const Alpha: Single);
var Item: TPHXSkinElement;
begin
  Item:= FParts[Element];

  if Assigned(Item) then
  begin
    Item.Draw(FCanvas, Position, Size, Alpha);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkin.Draw(Element: TPHXThemedPart; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single);
var Item: TPHXSkinElement;
begin
  Item:= FParts[Element];

  if Assigned(Item) then
  begin
    Item.Draw(FCanvas, Transform, Size, Alpha);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSkin.FindElement(const Name: String): TPHXSkinElement;
var Element: TPHXSkinElement;
begin
  if Elements.Find(Name, Element) then
  begin
    Result:= Element;
    Exit;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXSkin.FindElement(const Name: String; Default: TPHXSkinElement): TPHXSkinElement;
var Element: TPHXSkinElement;
begin
  if Elements.Find(Name, Element) then
  begin
    Result:= Element;
    Exit;
  end;
  Result:= Default;
end;



//------------------------------------------------------------------------------
function TPHXSkin.ElementAt(const X, Y: Integer): Integer;
var Index  : Integer;
var Element: TPHXSkinElement;
begin
  for Index := 0 to Elements.FList.Count - 1 do
  begin
    Element:= TPHXSkinElement(Elements.FList.List[Index]);

    if PointInRect(X,Y, Element.Bounds) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXSkin.GetPart(const Index: TPHXThemedPart): TPHXSkinElement;
begin
  Result:= FParts[Index];
end;

{$ENDREGION}

{$REGION 'TPHXSkinTransition'}

// TPHXSkinTransition
//------------------------------------------------------------------------------
constructor TPHXSkinTransition.Create;
begin
  FElement := nil;
  FPrevious:= nil;
  FActive  := False;
  FTime    := 0.0;
  FPosition:= 0.0;
  FDuration:= 0.5;
end;

//------------------------------------------------------------------------------
constructor TPHXSkinTransition.Create(AElement: TPHXSkinElement);
begin
  FElement := AElement;
  FPrevious:= nil;
  FActive  := False;
  FTime    := 0.0;
  FPosition:= 0.0;
  FDuration:= 0.5;
end;

//------------------------------------------------------------------------------
destructor TPHXSkinTransition.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinTransition.Update(const DeltaTime: Double);
begin
  if Assigned(FPrevious) then
  begin
    FTime    := FTime + DeltaTime;
    FPosition:= FTime * FDurationInv;

    if FTime > FDuration then
    begin
      FPrevious:= nil;
      FPosition:= 1.0;
      FActive  := False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinTransition.Draw(Canvas: TPHXCanvas; const Position: TVector3f; const Size: TVector2f; const Alpha: Single);
begin
  // Draw both the previous and current elements
  if Assigned(FPrevious) then
  begin
    FPrevious.Draw(Canvas, Position, Size, Alpha * (1 - FPosition));

    if Assigned(FElement) then
    begin
      FElement.Draw(Canvas, Position, Size, Alpha *  FPosition);
    end;
  end else
  // Draw only current
  if Assigned(FElement) then
  begin
    FElement.Draw(Canvas, Position, Size, Alpha);
  end;
end;
       (*
//------------------------------------------------------------------------------
procedure TPHXSkinTransition.Draw(Canvas: TPHXCanvas; const Transform: TMatrix4f; const Size: TVector2f; const Alpha: Single);
begin
  // Draw both the previous and current elements
  if Assigned(FPrevious) then
  begin
    FPrevious.Draw(Canvas, Transform, Size, Alpha * (1 - FPosition));

    if Assigned(FElement) then
    begin
      FElement.Draw(Canvas, Transform, Size, Alpha *  FPosition);
    end;
  end else
  // Draw only current
  begin
    FElement.Draw(Canvas, Transform, Size, Alpha);
  end;
end;  *)

//------------------------------------------------------------------------------
procedure TPHXSkinTransition.SetElement(const Value: TPHXSkinElement);
begin
  if FElement <> Value then
  begin

    // A transition is already active
    if Assigned(FPrevious) then
    begin
      FPrevious:= FElement;
      FTime    := FDuration - FTime;
      FPosition:= 1         - FPosition;
    end else
    begin
      FPrevious:= FElement;
      FTime    := 0.0;
      FPosition:= 0.0;
    end;

    FElement := Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinTransition.SetDuration(const Value: Single);
begin
  if Value > 0 then
  begin
    FDuration   := Value;
    FDurationInv:= 1 / Value;
  end;
end;

{$ENDREGION}



end.
