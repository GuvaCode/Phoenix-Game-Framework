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
unit phxProviderSelector;
//< Contains a dialog for selecting the provider and resolution to use

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  // For EnumDisplaySettings
  Windows,

  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}

  // For the form controls
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,

  phxTypes,
  phxClasses,
  phxDevice;

type

// Provides a dialog to select the provider and resolution
//------------------------------------------------------------------------------
TPHXProviderDialog = class
  public
    // Show a dialog to select a provider
    class function Show(out Provider: TPHXProvider): Boolean; overload; static;
    // Show a dialog to select a provider
    class function Show(out Provider: TPHXProvider; out Mode: TPHXDisplayMode): Boolean; overload; static;
  end;

// The form to use for the provider dialog
//------------------------------------------------------------------------------
TPHXProviderForm = class
  private
    ButtonOk    : TButton;
    ButtonCancel: TButton;

    edFullscreen: TCheckBox;
    edResolution: TComboBox;
    edProvider  : TComboBox;

    procedure CreateControls(Form: TForm);

    procedure edProviderChange(Sender: TObject);
    procedure edResolutionChange(Sender: TObject);
  protected
    procedure EnumDisplayModes;
  public
    // Default constructor
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Show the provider form
    function Execute(out Provider: TPHXProvider; out Mode: TPHXDisplayMode): Boolean;
  end;


implementation


{$R 'phxProviderSelector.res'}

//------------------------------------------------------------------------------
function ParseResolution(const Text: String): TSizei;
var P: Integer;
begin
  p:= Pos('x', Text);

  Result.Width := StrToInt( Copy(Text, 1  , P-1) );
  Result.Height:= StrToInt( Copy(Text, P+1, MaxInt) );
end;

// TPHXProviderDialog
//------------------------------------------------------------------------------
class function TPHXProviderDialog.Show(out Provider: TPHXProvider): Boolean;
var Form: TPHXProviderForm;
var Mode: TPHXDisplayMode;
begin
  Application.Initialize;

  Form:= TPHXProviderForm.Create;
  try
    // Show the form
    Result:= Form.Execute(Provider, Mode);
  finally
    Form.Free;
  end;
end;

//------------------------------------------------------------------------------
class function TPHXProviderDialog.Show(out Provider: TPHXProvider; out Mode: TPHXDisplayMode): Boolean;
var Form: TPHXProviderForm;
begin
  Application.Initialize;

  Form:= TPHXProviderForm.Create;
  try
    // Show the form
    Result:= Form.Execute(Provider, Mode);
  finally
    Form.Free;
  end;
end;


// TPHXProviderForm
//------------------------------------------------------------------------------
constructor TPHXProviderForm.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXProviderForm.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXProviderForm.Execute(out Provider: TPHXProvider; out Mode: TPHXDisplayMode): Boolean;
var Form: TForm;
begin
  Form:= TForm.CreateNew(nil, 0);
  try
    CreateControls(Form);

    EnumProviders(edProvider.Items);
    EnumDisplayModes;

    // Show the form
    Result:= Form.ShowModal = mrOk;

    if Result then
    begin
      Provider:= CreateProvider(edProvider.Text);

      Mode.Description:= '';
      Mode.Width      := ParseResolution(edResolution.Text).Width;
      Mode.Height     := ParseResolution(edResolution.Text).Height;
//      Mode.Fullscreen := edFullscreen.Checked;
    end else
    begin
      Provider:= nil;

      Mode.Description:= '';
      Mode.Width      := 0;
      Mode.Height     := 0;
//      Mode.Fullscreen := False;
    end;

  finally
    Form.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXProviderForm.CreateControls(Form: TForm);
var Panel: TPanel;
begin
  Form.Position   := poScreenCenter;
  Form.BorderStyle:= bsDialog;
  Form.Caption    := 'Phoenix Game Framework';
  Form.ClientWidth := 250 + 16;
  Form.ClientHeight:= 272 + 8;

  Form.Icon.LoadFromResourceName(HInstance, 'PHOENIX_ICON');

  // Create logo bitmap
  with TImage.Create(Form) do
  begin
    Picture.Bitmap.LoadFromResourceName(HInstance,'PHOENIX_LOGO');

    Parent:= Form;
    Left  := 8;
    Top   := 8;
    Width := 250;
    Height:= 137;
  end;


  Panel:= TPanel.Create(Form);
  with Panel do
  begin
    Parent     := Form;
    Left       := 8;
    Top        := 151;
    Width      := 250;
    Height     := 89;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
    TabOrder   := 2
  end;
  with TLabel.Create(Form) do
  begin
    Parent  := Panel;
    Left    := 8;
    Top     := 13;
    Width   := 40;
    Height  := 11;
    Caption := 'Provider';
  end;
  with TLabel.Create(Form) do
  begin
    Parent  := Panel;
    Left    := 8;
    Top     := 40;
    Width   := 54;
    Height  := 13;
    Caption := 'Resolution:';
  end;

  edFullscreen:= TCheckBox.Create(Form);
  with edFullscreen do
  begin
    Parent  := Panel;
    Left    := 68;
    Top     := 64;
    Width   := 97;
    Height  := 17;
    Caption := 'Fullscreen';
    TabOrder:= 0;
  end;

  edResolution:= TComboBox.Create(Form);
  with edResolution do
  begin
    Parent  := Panel;
    Left    := 68;
    Top     := 37;
    Width   := 173;
    Height  := 21;
    Style   := csDropDownList;
    TabOrder:= 1;
    OnChange:= edResolutionChange;
  end;

  edProvider:= TComboBox.Create(Form);
  with edProvider do
  begin
    Parent  := Panel;
    Left    := 68;
    Top     := 10;
    Width   := 173;
    Height  := 21;
    Style   := csDropDownList;
    TabOrder:= 2;
    OnChange:= edProviderChange;
  end;


  ButtonOk:= TButton.Create(Form);
  with ButtonOk do
  begin
    Parent:= Form;
    Left := 181;
    Top := 247;
    Width := 75;
    Height := 25;
    Anchors := [akRight, akBottom];
    Caption := 'Start';
    Default := True;
    Enabled := False;
    ModalResult := 1;
    TabOrder := 0;
  end;
  ButtonCancel:= TButton.Create(Form);
  with ButtonCancel do
  begin
    Parent:= Form;
    Left := 100;
    Top := 247;
    Width := 75;
    Height := 25;
    Anchors := [akRight, akBottom];
    Cancel := True;
    Caption := 'Exit';
    ModalResult := 2;
    TabOrder := 1;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXProviderForm.EnumDisplayModes;
var Status     : Boolean;
var Index      : Integer;
var DeviceMode : TDevMode;
var Description: String;
begin
  Index:=1;

  // enumerate all available video modes
  Status:= EnumDisplaySettings(nil, Index, DeviceMode);
  while Status do
  begin
    Description:=Format('%dx%d',[DeviceMode.dmPelsWidth, DeviceMode.dmPelsHeight]);

    if edResolution.Items.IndexOf(Description) = -1 then
    begin
      edResolution.Items.Add(Description);
    end;

    Inc(Index);

    Status:= EnumDisplaySettings(nil, Index, DeviceMode);
  end;

  Description:=Format('%dx%d',[Screen.Width, Screen.Height]);

  edResolution.ItemIndex:= edResolution.Items.IndexOf(Description);
end;


//------------------------------------------------------------------------------
procedure TPHXProviderForm.edProviderChange(Sender: TObject);
begin
  ButtonOk.Enabled:= (edProvider.ItemIndex >= 0) and (edResolution.ItemIndex >= 0);
end;

//------------------------------------------------------------------------------
procedure TPHXProviderForm.edResolutionChange(Sender: TObject);
begin
  ButtonOk.Enabled:= (edProvider.ItemIndex >= 0) and (edResolution.ItemIndex >= 0);
end;





end.
