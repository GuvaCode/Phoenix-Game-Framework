unit Tiles;

interface

uses Classes, SysUtils,
    phxTypes,
    phxClasses,
    phxImage,
    phxSprite,
    phxShape,

    phxSimpleXML;

const
  GroupSprites = cgGroup1;
  GroupTiles   = cgGroup2;

  OffsetY = 2*32;

type

// Tile sprite
TTile = class(TPHXSprite)

end;

// Apple sprite
TApple = class(TPHXSprite)

end;

// Banana sprite
TBanana = class(TPHXSprite)

end;
// Powerup sprite
TPowerup = class(TPHXAnimatedSprite)

end;

//------------------------------------------------------------------------------

{ TMap }

TMap = class
  private
    FWidth : Integer;
    FHeight: Integer;

    FTileSet   : TPHXImage;
    FTileWidth : Integer;
    FTileHeight: Integer;

    FApples : TList;
    FBananas: TList;


    function IsCollider(Tile: Integer): Boolean;

    procedure CreateTile(Engine: TPHXSpriteEngine; X, Y: Integer; const Tile: Integer);

    // Load a tile layer from the map file
    procedure LoadLayer(Engine: TPHXSpriteEngine; Node: TXMLNode);
    procedure LoadObjects(Engine: TPHXSpriteEngine; Group: TXMLNode);
  public
    constructor Create(ATileSet: TPHXImage);
    destructor Destroy; override;


    procedure LoadMap(const FileName: String; Engine: TPHXSpriteEngine);

    property Width : Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;

    property TileSet: TPHXImage read FTileSet write FTileSet;
    property TileWidth: Integer read FTileWidth write FTileWidth;
    property TileHeight: Integer read FTileHeight write FTileHeight;

    property Apples : TList read FApples;
    property Bananas: TList read FBananas;
  end;


implementation

// Tile indices
// 1  = Background
// 9  = Wall
// 17 = Floor
// 25 = Roof
// 33 = Wall
// 41 = Wall
// 49 = Wall
// 57 = Wall
//------------------------------------------------------------------------------
constructor TMap.Create(ATileSet: TPHXImage);
begin
  FTileSet:= ATileSet;
  FTileWidth:= 32;
  FTileHeight:= 32;

  FWidth := 100;
  FHeight:= 18;
  FApples  := TList.Create;
  FBananas := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TMap.Destroy;
begin
  FApples.Free;
  FBananas.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TMap.IsCollider(Tile: Integer): Boolean;
begin
  Result:= (Tile = 9) or (Tile = 17) or (Tile = 33) or (Tile = 41) or (Tile = 49) or (Tile = 57);
end;

//------------------------------------------------------------------------------
procedure TMap.CreateTile(Engine: TPHXSpriteEngine; X,Y: Integer; const Tile: Integer);
var Sprite: TPHXSprite;
begin
  Sprite:= TTile.Create(Engine);
  Sprite.Name    := IntToStr(Tile);
  Sprite.X       := X * TileWidth;
  Sprite.Y       := Y * TileHeight + OffsetY;

  if IsCollider(Tile) then
  begin
    Sprite.Collider:= True;
    Sprite.Mode    := cmStatic;
    Sprite.Group   := GroupTiles;
  end;

  Sprite.LinkedImage  := TileSet;
  Sprite.LinkedPattern:= Tile - 1;

  Sprite.Parent  := Engine.Root;
end;



//var A,B: AnsiString;
// <objectgroup name="Apples" width="100" height="18"/>
// <objectgroup name="Bananas" width="100" height="18">
//  <object name="Banana1" type="Banana" x="64" y="192" width="32" height="32"/>
// </objectgroup>

//------------------------------------------------------------------------------
procedure TMap.LoadLayer(Engine: TPHXSpriteEngine; Node: TXMLNode);
var Values: TStrings;
var X,Y   : Integer;
var Tile  : Integer;
begin
  Values:= TStringList.Create;
  try
    Values.Delimiter:= ',';
    Values.DelimitedText:= Node.Value;

    for Y:= 0 to FHeight-1 do
    begin
      for X:= 0 to FWidth-1 do
      begin
        Tile:= StrToInt(Values[X + Y * FWidth]);

        if Tile > 0 then
        begin
          CreateTile(Engine, X,Y, Tile);
        end;
      end;
    end;
  finally
    Values.Free;
  end;
end;

// <objectgroup name="Pickups" width="100" height="18">
//   <object name="B1" type="Banana" x="112" y="464"/>
//   <object name="B2" type="Banana" x="784" y="528"/>
//   <object name="B3" type="Banana" x="80" y="144"/>
//   <object name="A1" type="Apple" x="176" y="464"/>
// </objectgroup>
//------------------------------------------------------------------------------
procedure TMap.LoadObjects(Engine: TPHXSpriteEngine; Group: TXMLNode);
var Index: Integer;
var Node : TXMLNode;
var Sprite: TPHXSprite;
begin
  for Index:= 0 to Group.ChildNodes.Count-1 do
  begin
    Node:= Group.ChildNodes[Index];

    if Node.Name <> 'object' then Continue;

    if Node.Attributes['type'] = 'Banana' then
    begin
      Sprite:= TBanana.Create(Engine);

      Bananas.Add(Sprite);

      if Node.HasAttribute('name') then
      begin
        Sprite.Name:= Node.Attributes['name'];
      end else
      begin
        Sprite.Name:= 'Banana' + IntToStr(Bananas.Count);
      end;

      Sprite.X   := Node.Attributes['x'];
      Sprite.Y   := Node.Attributes['y'] + OffsetY;

      Sprite.Collider:= True;
      Sprite.Mode    := cmStatic;
      Sprite.Group   := GroupSprites;

      Sprite.Image  := 'Sprites';
      Sprite.Pattern:= 'Banana';

      Sprite.Parent := Engine.Root;

    end;

    if Node.Attributes['type'] = 'Apple' then
    begin
      Sprite:= TApple.Create(Engine);

      Apples.Add(Sprite);

      if Node.HasAttribute('name') then
      begin
        Sprite.Name:= Node.Attributes['name'];
      end else
      begin
        Sprite.Name:= 'Apple' + IntToStr(Apples.Count);
      end;

      Sprite.X   := Node.Attributes['x'];
      Sprite.Y   := Node.Attributes['y'] + OffsetY;

      Sprite.Collider:= True;
      Sprite.Mode    := cmStatic;
      Sprite.Group   := GroupSprites;

      Sprite.Image  := 'Sprites';
      Sprite.Pattern:= 'Apple';

      Sprite.Parent := Engine.Root;

    end;
    if Node.Attributes['type'] = 'Powerup' then
    begin
      Sprite:= TPowerup.Create(Engine);
      Sprite.Name:= 'Powerup';

      Sprite.X   := Node.Attributes['x'];
      Sprite.Y   := Node.Attributes['y'] + OffsetY;

      Sprite.Collider:= True;
      Sprite.Mode    := cmStatic;
      Sprite.Group   := GroupSprites;

      TPHXAnimatedSprite(Sprite).Image   := 'Sprites';
      TPHXAnimatedSprite(Sprite).Animation:= 'Powerup';

      Sprite.Parent := Engine.Root;
    end;


  end;
end;

//------------------------------------------------------------------------------
procedure TMap.LoadMap(const FileName: String; Engine: TPHXSpriteEngine);
var Document  : TXMLDocument;
var NodeLayer : TXMLNode;
var NodeData  : TXMLNode;
var NodeObjects: TXMLNode;

begin
//  A:= Base64Encode(Data);
 // B:= Base64Decode(A);

  Document:= TXMLDocument.Create;
  try
    Document.LoadFromFile(FileName);

    NodeLayer:= Document.Root.Nodes.Find('layer');

    if Assigned(NodeLayer) then
    begin
      FWidth := NodeLayer.Attributes['width'];
      FHeight:= NodeLayer.Attributes['height'];

      NodeData:= NodeLayer.FindChild('data');

      if Assigned(NodeData) then
      begin
        if NodeData.Attributes['encoding'] <> 'csv' then
        begin
          raise Exception.Create('Only csv encoding supported');
        end;

        LoadLayer(Engine, NodeData);
      end else
      begin
        raise Exception.Create('Could not find the data node');
      end;

    end else
    begin
       raise Exception.Create('Could not find the layer node');
    end;

    NodeObjects:= Document.Root.Nodes.Find('objectgroup');
    if Assigned(NodeObjects) then
    begin
      LoadObjects(Engine, NodeObjects);
    end;


  finally
    Document.Free;
  end;

end;

(*

// Uses Graphics, clipBrd;
// 1  = Background
// 9  = Wall
// 17 = Floor
// 25 = Roof
// 33 = Wall
// 41 = Wall
// 49 = Wall
// 57 = Wall
// Generates tiled map data map from fgland.bmp
//------------------------------------------------------------------------------
procedure GenMap;
var Bitmap: TBitmap;
var X,Y   : Integer;
var Color : TColor;
var Tile  : Integer;
var Map   : String;
begin
  Bitmap:= TBitmap.Create;
  Bitmap.LoadFromFile('content/platformer/fgland.bmp');


  Map:= '';
  for Y:=0 to Bitmap.Height-1 do
  begin
    for X:=0 to Bitmap.Width-1 do
    begin
      Color:= Bitmap.Canvas.Pixels[X,Y];
      Tile := 0;

      case Color of
        /// Air
        clWhite:
        begin
          Tile:= 0;
        end;
        // Floor tile
        clBlack :
        begin
          Tile:= 17;
        end;
        // Background tile
        clRed:
        begin
          Tile:= 1;
        end;
        // Roof tile
        clBlue:
        begin
          Tile:= 25;
        end;
        // Random filler tile
        clGreen:
        begin
         case Random(5) of
           0: Tile:= 9;
           1: Tile:= 33;
           2: Tile:= 41;
           3: Tile:= 49;
           4: Tile:= 57;
         end;
        end;
        // Banana
        clFuchsia:
        begin
          Tile:= 0;
        end;
        // Apple
        clAqua:
        begin
          Tile:= 0;
        end;
      end;

      Map:= Map + IntToStr(Tile)+ ',';
    end;
    Map:= Map + #13;
  end;
  Bitmap.Free;

  Map:=Copy(Map, 1, Length(Map) - 2);

  Clipboard.AsText:= Map;

end;
*)


end.
