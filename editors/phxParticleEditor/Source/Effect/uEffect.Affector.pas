unit uEffect.Affector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls, Vcl.ImgList, Vcl.ActnList,

  Generics.Collections,

  phxParticle,
  phxParticleAffectors, Vcl.ExtCtrls;

type

//------------------------------------------------------------------------------
IAffectorEditor = interface
  ['{ACA0F99B-6636-4B45-A3A0-8E10D8B9672C}']

  function GetTitle: String;

  procedure SetAffector(Value: TPHXParticleAffector);
end;

TFrameClass = class of TFrame;

//------------------------------------------------------------------------------
TFrmEffectAffector = class(TFrame)
    ImageList1: TImageList;
    MenuAffectors: TPopupMenu;
    ActionList1: TActionList;
    actAffectorAdd: TAction;
    actAffectorDel: TAction;
    Splitter1: TSplitter;
    MenuAffectorsItem: TMenuItem;
    Panel1: TPanel;
    lwAffectors: TListView;
    ToolBar3: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    GroupBox2: TGroupBox;
    procedure MenuAffectorsPopup(Sender: TObject);
    procedure actAffectorAddExecute(Sender: TObject);
    procedure actAffectorDelExecute(Sender: TObject);
    procedure lwAffectorsSelectItem(Sender: TObject; Item: TListItem;   Selected: Boolean);
    procedure MenuAffectorsItemClick(Sender: TObject);
    procedure actAffectorAddUpdate(Sender: TObject);
    procedure actAffectorDelUpdate(Sender: TObject);
  private
    FEffect  : TPHXParticleEffect;
    FAffector: TPHXParticleAffector;
    FEditor  : TFrame;

    procedure lwAffectorsUpdate;

    procedure SetEffect(const Value: TPHXParticleEffect);
    procedure SetAffector(const Value: TPHXParticleAffector);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
    property Affector: TPHXParticleAffector read FAffector write SetAffector;
  end;

procedure RegisterAffectorEditor(Affector: TPHXParticleAffectorClass; Editor: TFrameClass);

implementation

{$R *.dfm}

uses uActions;

var AffectorRegistry: TDictionary<TPHXParticleAffectorClass, TFrameClass>;

//------------------------------------------------------------------------------
procedure RegisterAffectorEditor(Affector: TPHXParticleAffectorClass; Editor: TFrameClass);
begin
  AffectorRegistry.Add(Affector, Editor);
end;

// TFrmEffectAffector
//------------------------------------------------------------------------------
constructor TFrmEffectAffector.Create(AOwner: TComponent);
begin
  inherited;

  SetEffect(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.lwAffectorsUpdate;
var Index   : Integer;
var Affector: TPHXParticleAffector;
var ListItem: TListItem;
begin
  lwAffectors.Items.Clear;
  lwAffectors.Items.BeginUpdate;
  for Index := 0 to Effect.Affectors.Count - 1 do
  begin
    Affector:= TPHXParticleAffector( Effect.Affectors[Index] );

    ListItem:= lwAffectors.Items.Add;
    ListItem.Caption:= IntToStr(Index);
    ListItem.SubItems.Add( Affector.Name );

    if FAffector = Affector then
    begin
      ListItem.Selected:= True;
    end;

  end;
  lwAffectors.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.lwAffectorsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var Index: Integer;
begin
  Index:= lwAffectors.ItemIndex;

  if Assigned(Effect) and (Index >= 0) and (Index < Effect.Affectors.Count) then
  begin
    SetAffector(Effect.Affectors[Index]);
  end else
  begin
    SetAffector(nil);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.MenuAffectorsPopup(Sender: TObject);
var Index: Integer;
var Item  : TMenuItem;
begin
  MenuAffectors.Items.Clear;
  for Index := 0 to getAffectorRegistryCount - 1 do
  begin
    Item:= TMenuItem.Create(Self);
    Item.Caption:= GetAffectorRegistryName(Index);
    Item.Tag    := Index;
    Item.OnClick:= MenuAffectorsItemClick;

    MenuAffectors.Items.Add(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.MenuAffectorsItemClick(Sender: TObject);
var Index   : Integer;
var Affector: TPHXParticleAffector;
begin
  Index:= TMenuItem(Sender).Tag;

  Affector:= FEffect.Affectors.Add(GetAffectorRegistryName(Index));

  if Assigned(Affector) then
  begin
    SetAffector(Affector);

    lwAffectorsUpdate;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.actAffectorAddUpdate(Sender: TObject);
begin
  actAffectorAdd.Enabled:= Assigned(Effect);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.actAffectorAddExecute(Sender: TObject);
begin
  //
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.actAffectorDelUpdate(Sender: TObject);
begin
  actAffectorDel.Enabled:= Assigned(Effect) and Assigned(Affector);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.actAffectorDelExecute(Sender: TObject);
begin
  if MessageDlg(Format('Delete the %s affector?', [Affector.Name]), mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    FEffect.Affectors.Remove(Affector);

    Affector:= nil;

    lwAffectorsUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.SetAffector(const Value: TPHXParticleAffector);
var EditorClass: TFrameClass;
var EditorIF: IAffectorEditor;
begin
  if FAffector <> Value then
  begin
    // Free the editor
    if Assigned(FEditor) then
    begin
      if Supports(FEditor, IAffectorEditor, EditorIF)  then
      begin
        EditorIF.SetAffector(nil);
      end;
      FEditor.Free;
      FEditor:= nil;
    end;

    FAffector := Value;

    if Assigned(FAffector) then
    begin
      EditorClass:= nil;

      if AffectorRegistry.ContainsKey( TPHXParticleAffectorClass(FAffector.ClassType)) then
      begin
        EditorClass:= AffectorRegistry[ TPHXParticleAffectorClass(FAffector.ClassType) ];
      end else
      if AffectorRegistry.ContainsKey( TPHXParticleAffectorClass(FAffector.ClassParent)) then
      begin
        EditorClass:= AffectorRegistry[ TPHXParticleAffectorClass(FAffector.ClassParent) ];
      end;

      if Assigned(EditorClass) then
      begin
        FEditor:= EditorClass.Create(Self);
        FEditor.Parent:= GroupBox2;
        FEditor.Align := alClient;

        if Supports(FEditor, IAffectorEditor, EditorIF)  then
        begin
          EditorIF.SetAffector(FAffector);

          GroupBox2.Caption:= EditorIF.GetTitle;
        end else
        begin
          GroupBox2.Caption:= FAffector.ClassName;
        end;

        GroupBox2.Visible := True;
      end else
      begin
        GroupBox2.Visible:= False;
      end;
    end else
    begin
      GroupBox2.Visible:= False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectAffector.SetEffect(const Value: TPHXParticleEffect);
begin
  if FEffect <> Value then
  begin
    SetAffector(nil);

    FEffect:= Value;

    if Assigned(FEffect) then
    begin
      lwAffectors.Enabled:= True;
      lwAffectors.Color  := clWindow;

      lwAffectorsUpdate;
    end else
    begin
      lwAffectors.Enabled:= False;
      lwAffectors.Color  := clBtnFace;

      lwAffectors.Items.Clear;
    end;
  end;

end;




initialization
  AffectorRegistry:= TDictionary<TPHXParticleAffectorClass, TFrameClass>.Create;
finalization
  AffectorRegistry.Free;
end.
