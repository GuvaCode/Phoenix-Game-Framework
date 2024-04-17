unit uPacker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ImgList, ExtCtrls, ToolWin, Menus, Mask,

  JvExMask, JvSpin,

  phxDevice,
  phxGraphics,
  phxGraphicsEx,

  phxTypes;

type

TPHXPackerItem = record
	// The name of the bitmap
	Name: String[32];
  // Width of the item
  Width: Integer;
  // Height of the item
  Height: Integer;
	// The bitmap to pack
	Bitmap: TPHXBitmap;
	// True if the bitmap was added to the target
	Placed: Boolean;
	// The position of the bitmap in the packed image
	Position: TVector2i;
end;

PPackerItemList = ^TPackerItemList;
TPackerItemList = Array[0.. $00FFFFFF] of TPHXPackerItem;

TPHXPackerItemList = class
	private
	  FCount: Integer;
		FCapacity: Integer;
		FList: PPackerItemList;

    Procedure Grow;

    function  GetItem(Index: Integer): TPHXPackerItem;
    procedure SetItem(Index: Integer; const Value: TPHXPackerItem);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
	public
    constructor Create;
    destructor Destroy; override;

    Procedure Clear;

	 	Procedure Sort;

     // Add a new pattern to the list
    Procedure Add(const Value: TPHXPackerItem ); overload;

    Procedure Add(const Name: String; Bitmap: TPHXBitmap ); overload;

    Procedure Delete(Index: Integer);

    // Returns the index of a pattern, -1 if not found.
    Function IndexOf(const Name: String): Integer;
  // Number of items in the pattern list
    Property Count: Integer read FCount write SetCount;
    // Capacity of the list
    Property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    Property List: PPackerItemList read FList;
    // Gets and sets the patterns
    Property Items[Index: Integer]: TPHXPackerItem read GetItem Write SetItem; default;
	end;



type
  TfrmPacker = class(TForm)
    ImageList1: TImageList;
    OpenImageDialog: TOpenDialog;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Panel2: TPanel;
    Panel1: TPanel;
    btnOk: TButton;
    Button4: TButton;
    ImageList2: TImageList;
    Panel3: TPanel;
    GroupBox2: TGroupBox;
    GroupBox1: TGroupBox;
    lwImages: TListView;
    ToolBar3: TToolBar;
    ButtonAdd: TToolButton;
    ButtonDel: TToolButton;
    btnPack: TButton;
    cbWidth: TComboBox;
    cbHeight: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbAutoSize: TCheckBox;
    PopupMenu1: TPopupMenu;
    btnClear: TMenuItem;
    Insert1: TMenuItem;
    Label3: TLabel;
    Splitter1: TSplitter;
    edPadding: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnPackClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure lwImagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure cbAutoSizeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
    FBuffer     : TBitmap;
    FTransparent : TBitmap;
    FItems: TPHXPackerItemList;

    FImage: TPHXBitmap;
    procedure DetermineSize(out AWidth: Integer; out AHeight: Integer);

    Procedure ClearImages;

    procedure UpdateImageList;
    procedure DrawPattern(Dest: TCanvas; Index: Cardinal);
  public
    { Public declarations }
    function ShowDialog: TModalResult;

     Property Image: TPHXBitmap read FImage;

     Property Items: TPHXPackerItemList read FItems;
  end;

var
  frmPacker: TfrmPacker;

implementation

{$R *.dfm}

uses Partitions;

procedure QuickSort(SortList: PPackerItemList; L, R: Integer);
var
  I, J: Integer;
  P, T: TPHXPackerItem;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (P.Width - SortList^[I].Width) < 0 do
        Inc(I);
      while (P.Width - SortList^[J].Width) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J);
    L := I;
  until I >= R;
end;


// TPHXPackerItemList
//==============================================================================
constructor TPHXPackerItemList.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXPackerItemList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Add(const Value: TPHXPackerItem );
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Add(const Name: String; Bitmap: TPHXBitmap);
begin
 SetCount(Count + 1);

  FList^[Count - 1].Name      := ShortString(Name);
  FList^[Count - 1].Width     := Bitmap.Width;
  FList^[Count - 1].Height    := Bitmap.Height;
  FList^[Count - 1].Bitmap    := Bitmap;
  FList^[Count - 1].Placed    := False;
  FList^[Count - 1].Position.X:= -1;
  FList^[Count - 1].Position.Y:= -1;
end;


//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Delete(Index: Integer);
begin
  If (Index < 0) or (Index >= FCount) then Exit;

  FCount := FCount-1;

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXPackerItem));
end;

//------------------------------------------------------------------------------
function TPHXPackerItemList.IndexOf(const Name: String): Integer;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;


//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXPackerItem));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXPackerItemList.GetItem(Index: Integer): TPHXPackerItem;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.SetItem(Index: Integer; const Value: TPHXPackerItem);
begin
  FList^[Index]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXPackerItemList.Sort;
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1);
end;


//------------------------------------------------------------------------------
Procedure TfrmPacker.DrawPattern(Dest: TCanvas; Index: Cardinal);
var Pattern: TPHXPackerItem ;
var rPattern: TRect;
begin
  Pattern:=Items[Index];

  rPattern.Left  := (Pattern.Position.X                 );
  rPattern.Top   := (Pattern.Position.Y                 );
  rPattern.Right := (Pattern.Position.X + Pattern.Width );
  rPattern.Bottom:= (Pattern.Position.Y + Pattern.Height);

  with Dest do begin
     Brush.Style:= bsClear;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.FormCreate(Sender: TObject);
var Index: Integer;
var Filter: String;
begin
  Filter:= 'All supported|';
  for Index := 0 to GraphicFormats.Count - 1 do
  begin
    Filter:=Filter + ';*'+ String(GraphicFormats[Index].Extension);
  end;
  OpenImageDialog.Filter:= Filter;

  FImage:=TPHXBitmap.Create;

  FItems :=TPHXPackerItemList.Create;
  FBuffer:= TBitmap.Create;

  FTransparent:= CreateTransparentImage(4);

  ScrollBox1.DoubleBuffered:= True;

  btnOk.Enabled:= False;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.FormDestroy(Sender: TObject);
begin
  ClearImages;

  FTransparent.Free;
  FImage.Free;
  FBuffer.Free;
  FItems.Free;
end;

//------------------------------------------------------------------------------
function TfrmPacker.ShowDialog: TModalResult;
begin
//  ClearImages;

  UpdateImageList;

  btnOk.Enabled:= False;

  Result:= ShowModal;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.UpdateImageList;
var Index: Integer;
var ListItem: TListItem;
begin
  lwImages.Items.BeginUpdate;
  lwImages.Items.Clear;

  for Index := 0 to Items.Count - 1 do
  begin
    ListItem:= lwImages.Items.Add;
    ListItem.Caption:= String(Items.List^[Index].Name);

    if Items.List^[Index].Placed then
    begin
      ListItem.ImageIndex:= 0;

      ListItem.SubItems.Add(IntToStr(Items.List^[Index].Position.X));
      ListItem.SubItems.Add(IntToStr(Items.List^[Index].Position.Y));
    end else
    begin
      ListItem.ImageIndex:= -1;

      ListItem.SubItems.Add('');
      ListItem.SubItems.Add('');
    end;

  end;

  lwImages.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.ButtonAddClick(Sender: TObject);
var Index: Integer;
var Filename: String;
var Bitmap  : TPHXBitmap;
begin
  if OpenImageDialog.Execute then
  begin
    for Index := 0 to OpenImageDialog.Files.Count - 1 do
    begin
      Filename:= OpenImageDialog.Files[Index] ;

      Bitmap:= TPHXBitmap.Create;
      Bitmap.LoadBitmap(Filename);

      Items.Add( ChangeFileExt(ExtractFileName(Filename), ''), Bitmap);
    end;
  end;

  for Index := 0 to Items.Count - 1 do
  begin
    Items.List^[Index].Placed:= False;
  end;
  btnOk.Enabled:= False;

  UpdateImageList;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.ButtonDelClick(Sender: TObject);
var Index: Integer;
begin
  if lwImages.SelCount = 0 then Exit;

  Items.List^[lwImages.Selected.Index].Bitmap.Free;

  Items.Delete(lwImages.Selected.Index);

  for Index := 0 to Items.Count - 1 do
  begin
    Items.List^[Index].Placed:= False;
  end;
  btnOk.Enabled:= False;

  UpdateImageList;
end;



//------------------------------------------------------------------------------
procedure TfrmPacker.cbAutoSizeClick(Sender: TObject);
begin
  cbWidth.Enabled := not cbAutoSize.Checked;
  cbHeight.Enabled:= not cbAutoSize.Checked;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.ClearImages;
var Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
  begin
    Items.List^[Index].Bitmap.Free;
  end;
  Items.Clear;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Width:= FBuffer.Width;
  PaintBox1.Height:= FBuffer.Height;

  PaintBox1.Canvas.Draw(0, 0, FBuffer);

  if btnOk.Enabled and (lwImages.SelCount > 0) then
  begin
    DrawPattern(PaintBox1.Canvas, lwImages.Selected.Index);
  end;
end;


//------------------------------------------------------------------------------
procedure TfrmPacker.btnPackClick(Sender: TObject);

var Partition: TPartition;
var Index    : Integer;
var ARect    : TRect;
var AllPacked: Boolean;

var Source: TRecti;
var Dest: TVector2i;
var AWidth  : Integer;
var AHeight : Integer;
var APadding: Integer;
begin
  Screen.Cursor:= crHourGlass;

  Items.Sort;

  if cbAutoSize.Checked then
  begin
    DetermineSize(AWidth, AHeight);
  end else
  begin
    AWidth := StrToIntDef( cbWidth.Text , 256);
    AHeight:= StrToIntDef( cbHeight.Text, 256);
  end;
  APadding := Round( edPadding.Value );


  Image.Resize(AWidth, AHeight, pfRGBA);
  Image.Fill(0, 0, 0, 0);

  Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);

  AllPacked:= True;
  for Index := 0 to Items.Count - 1 do
  begin
    Items.List^[Index].Placed:= Partition.Insert(Items[Index].Width + APadding, Items[Index].Height + APadding, ARect);

    if Items.List^[Index].Placed then
    begin
      Source.Left  := 0;
      Source.Top   := 0;
      Source.Right := Items[Index].Width - 1;
      Source.Bottom:= Items[Index].Height - 1;

      Dest.X:= ARect.Left;
      Dest.Y:= ARect.Top;

      Image.CopyFrom(Items.List^[Index].Bitmap, Source, Dest);

      Items.List^[Index].Position.X:= ARect.Left;
      Items.List^[Index].Position.Y:= ARect.Top;
    end else
    begin
      AllPacked:= False;
    end;
  end;
  Partition.Free;

  FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);

  Image.Draw(FBuffer, FTransparent);

  UpdateImageList;

  ScrollBox1.Invalidate;
  PaintBox1.Invalidate;

  Screen.Cursor:= crDefault;

  if not AllPacked then
  begin
    MessageDlg('There was not enough space in the destination image to pack'#13'all the images.', mtInformation, [mbOK], 0)
  end;


  btnOk.Enabled:= True;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.DetermineSize(out AWidth, AHeight: Integer);
var Partition: TPartition;
var Index    : Integer;
var ARect    : TRect;
var Done     : Boolean;
var APadding : Integer;
begin
  APadding := Round( edPadding.Value );

  AWidth := 16;
  AHeight:= 16;
  repeat
    Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);

    Done:= True;
    for Index := 0 to Items.Count - 1 do
    begin
      Done:= Done and Partition.Insert(Items[Index].Width + APadding, Items[Index].Height + APadding, ARect);

      if not Done then Break;
    end;
    Partition.Free;

    if Done then Exit;

    if AWidth <= AHeight then
    begin
      AWidth := AWidth  * 2;
    end else
    begin
      AHeight:= AHeight * 2;
    end;

  until Done;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.lwImagesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (not btnOk.Enabled) and Selected then
  begin
    Items.List^[Item.Index].Bitmap.Draw(FBuffer, FTransparent);
  end;
  ScrollBox1.Invalidate;
//  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.btnClearClick(Sender: TObject);
var Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
  begin
    Items.List^[Index].Bitmap.Free;
    Items.List^[Index].Placed:= False;
  end;
  Items.Clear;
  btnOk.Enabled:= False;

  UpdateImageList;
end;

end.
