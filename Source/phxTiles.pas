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
unit phxTiles;
//< Classes and functions for tile map rendering

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxClasses,
  phxCanvas,
  phxImage;


const
  // TODO: Temporary
  TileWidth  = 64;
  TileHeight = 32;

  MapWidth  = 16;
  MapHeight = 16;

  MapOffsetX = 400;
  MapOffsetY = 100;

  TILE_LAYERS = 4;

const
  NONE_TILE = 65535;

type

TPHXTileMap = class;

TPHXTile = Word;

PTileList = ^TTileList;
TTileList = array[0..$00FFFFFF] of TPHXTile;

// A single layer in a tile map
//------------------------------------------------------------------------------
TPHXTileLayer = class
  private
    FName       : String;
    FDescription: String;
    FWidth      : Integer;
    FHeight     : Integer;
    FVisible    : Boolean;
    FList       : PTileList;

    function GetTile(const X, Y: Integer): TPHXTile;

    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetTile(const X, Y: Integer; const Value: TPHXTile);
  public
    constructor Create; overload;
    constructor Create(const AWidth, AHeight: Integer); overload;
    destructor Destroy; override;

    // Resize the tile layer
    procedure Resize(const AWidth, AHeight: Integer);

    // Fill the layer with a tile
    procedure Fill(const ATile: TPHXTile);

    //procedure PathFind(const Source: TVector2i; const Destination: TVector2i

    // Name of the layer
    property Name: String read FName write FName;
    // Description of the layer
    property Description: String read FDescription write FDescription;
    // Width of the layer
    property Width: Integer read FWidth write SetWidth;
    // Height of the layert
    property Height: Integer read FHeight write SetHeight;
    // Height of the layert
    property Visible: Boolean read FVisible write FVisible;
    // Pointer to the list of tiles
    property List: PTileList read FList;
    // Set or get a tile
    property Tile[const X, Y: Integer]: TPHXTile read GetTile write SetTile; default;
  end;

//------------------------------------------------------------------------------
TPHXTileSet = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FImage: TPHXImage;
  public
    property Image: TPHXImage read FImage write FImage;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;
// http://old.cokeandcode.com/pathfinding

// Abstract tile map
//------------------------------------------------------------------------------
TPHXTileMap = class
  private
    FName       : String;
    FDescription: String;
    FWidth      : Integer;
    FHeight     : Integer;

    FLayer: TPHXTileLayer;

    FTileHeight: Integer;
    FTileWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(const AWidth, AHeight: Integer);
    destructor Destroy; override;

    // Returns true if a tile in the map is blocked
    function Blocked(const X,Y: Integer): Boolean; virtual;

    procedure Render(Canvas: TPHXCanvas);

    // Converts a screen coordinate to a world coordinate
    function ScreenToWorld(const Value: TVector2f): TVector2f; virtual;
    function WorldToScreen(const Value: TVector2f): TVector2f; virtual;


    // Name of the tile map
    property Name: String read FName write FName;
    // Description of the tile map
    property Description: String read FDescription write FDescription;
    // Width of the tile map
    property Width: Integer read FWidth write SetWidth;
    // Height of the tile map
    property Height: Integer read FHeight write SetHeight;


    property TileWidth: Integer read FTileWidth write FTileWidth;
    property TileHeight: Integer read FTileHeight write FTileHeight;

    property Layer: TPHXTileLayer read FLayer;
  end;


















// This descibes a neighbour to a tile
//------------------------------------------------------------------------------
TPHXTileNeighbour = record
  DX: Integer;
  DY: Integer;
  Cost: Integer;
end;



//------------------------------------------------------------------------------
TPHXTilePath = class
  private
    FSteps: TPHXVectorList2i;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Render(Canvas: TPHXCanvas; Map: TPHXTileMap);

    property Steps: TPHXVectorList2i read FSteps;
  end;

//------------------------------------------------------------------------------
TPHXPathNode = class
  public
    X: Integer;
    Y: Integer;
    Parent: TPHXPathNode;
    Cost: Integer;
    Heuristic: Integer;
    Depth: Integer;
    Closed: Boolean;
    // Number of times the node has been added to the open list
    Open  : Integer;
  public
    constructor Create(const AX, AY: Integer);
    function ToString: string; override;


  end;

PPathNodeList = ^TPathNodeList;
TPathNodeList = array[0..$00FFFFFF] of TPHXPathNode;

// Class for finding a path between two points on a tile map
//------------------------------------------------------------------------------
TPHXPathFinder = class
  private
    FMap  : TPHXTileMap;

    // 2D array of all nodes in the tile map
    Nodes: array of array of TPHXPathNode;

    Open: TList;

    FOpenCount   : Integer;
    FOpenCapacity: Integer;
    FOpenList    : PPathNodeList;

    function InOpenList(Node: TPHXPathNode): Boolean;

    // Initialise all nodes
    procedure Init(Layer: TPHXTileLayer);
    // Reset all nodes
    procedure Reset(const TX, TY: Integer);

    // Add a node to the open list
    procedure AddToOpen(Node: TPHXPathNode);
    // Remove a node to the open list
    procedure RemoveFromCurrent(Node: TPHXPathNode);
  public
    constructor Create(AMap: TPHXTileMap);
    destructor Destroy; override;

    // Finds a path in the tilemap
    function FindPath(const SX, SY, TX, TY: Integer; const Path: TPHXTilePath): Boolean;

    property Map: TPHXTileMap read FMap;
  end;













//------------------------------------------------------------------------------
TPHXRectangularMap = class(TPHXTileMap)
  private
  public
    constructor Create;
  end;


// Class for isometric tiles
//------------------------------------------------------------------------------
TPHXIsometricMap = class//(TPHXTileMap)
  public
    function WorldToScreen(const X, Y : Single): TVector2f; overload;
    function WorldToScreen(const Value: TVector2f): TVector2f; overload;

    function ScreenToWorld(const X, Y : Single): TVector2f; overload;
    function ScreenToWorld(const Value: TVector2f): TVector2f; overload;
  end;

// http://jemgine.omnisu.com/?page_id=412
THexDirection = (
  NorthEast,
	East,
	SouthEast,
	SouthWest,
	West,
	NorthWest
);

const

//------------------------------------------------------------------------------
Neighbours: array[0..3] of TPHXTileNeighbour =
(
  (DX:   0; DY: - 1; Cost: 10), //90° neighbour cubes
  (DX: - 1; DY:   0; Cost: 10),
  (DX: + 1; DY:   0; Cost: 10),
  (DX:   0; DY: + 1; Cost: 10)
);


implementation

// TPHXTileLayer
//==============================================================================
constructor TPHXTileLayer.Create;
begin
  FName   := 'Layer';
  FWidth  := 0;
  FHeight := 0;
  FVisible:= True;
end;

//------------------------------------------------------------------------------
constructor TPHXTileLayer.Create(const AWidth, AHeight: Integer);
begin
  FName   := 'Layer';
  FWidth  := AWidth;
  FHeight := AHeight;
  FVisible:= True;

  Resize(AWidth, AHeight);
end;

//------------------------------------------------------------------------------
destructor TPHXTileLayer.Destroy;
begin
  ReallocMem(FList, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXTileLayer.Resize(const AWidth, AHeight: Integer);
begin
  FWidth:= AWidth;
  FHeight:= AHeight;

  ReallocMem(FList, FWidth * FHeight * SizeOf(TPHXTile));
end;

//------------------------------------------------------------------------------
procedure TPHXTileLayer.Fill(const ATile: TPHXTile);
var Index: Integer;
begin
  for Index := 0 to (FWidth * FHeight) - 1 do
  begin
    FList^[Index]:= ATile;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTileLayer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;

    Resize(FWidth, FHeight);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTileLayer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;

    Resize(FWidth, FHeight);
  end;
end;

//------------------------------------------------------------------------------
function TPHXTileLayer.GetTile(const X, Y: Integer): TPHXTile;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    Result:= FList[X + Y * Width];
  end else
  begin
    Result:= NONE_TILE;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTileLayer.SetTile(const X, Y: Integer; const Value: TPHXTile);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    FList[X + Y * Width]:= Value;
  end;
end;


{ TPHXTilePath }

constructor TPHXTilePath.Create;
begin
  FSteps:= TPHXVectorList2i.Create;
end;

destructor TPHXTilePath.Destroy;
begin
  FSteps.Free;
end;

procedure TPHXTilePath.Render(Canvas: TPHXCanvas; Map: TPHXTileMap);
var Index: Integer;
var worldX: Integer;
var worldY: Integer;
var screenX: Integer;
var screenY: Integer;
begin
  for Index := 0 to FSteps.Count - 1 do
  begin
    worldX:= FSteps[Index].X;
    worldY:= FSteps[Index].Y;

    screenX:= worldX * Map.TileWidth;
    screenY:= worldY * Map.TileHeight;

    Canvas.Rectangle(screenX, screenY, screenX + Map.TileWidth, screenY + Map.TileHeight);
  end;
end;



{ TPHXTileMap }

constructor TPHXTileMap.Create(const AWidth, AHeight: Integer);
begin
  FLayer:= TPHXTileLayer.Create(AWidth, AHeight);

  FTileWidth := 16;
  FTileHeight:= 16;
end;

destructor TPHXTileMap.Destroy;
begin
  FLayer.Free;
end;

function TPHXTileMap.Blocked(const X, Y: Integer): Boolean;
begin
  Result:= FLayer[X,Y] > 0;
end;

procedure TPHXTileMap.Render(Canvas: TPHXCanvas);
var worldX: Integer;
var worldY: Integer;
var screenX: Integer;
var screenY: Integer;
var Tile: TPHXTile;
begin
  for worldY:= 0 to FLayer.Height-1 do
  begin
    screenY:= worldY * TileHeight;

    for worldX := 0 to FLayer.Width - 1 do
    begin
      Tile:= FLayer.List^[worldX + worldY * FLayer.Width];

      screenX:= worldX * TileWidth;


      if Tile = 0 then
      begin
        Canvas.Rectangle(screenX, screenY, screenX + TileWidth, screenY + TileHeight);
      end else
      begin
        Canvas.FilledRectangle(screenX, screenY, screenX + TileWidth, screenY + TileHeight);
      end;


    end;

 //   Canvas.Flush;
  end;
end;


function TPHXTileMap.ScreenToWorld(const Value: TVector2f): TVector2f;
begin
  Result.X:= Value.X / FTileWidth;
  Result.Y:= Value.Y / FTileHeight;
end;

function TPHXTileMap.WorldToScreen(const Value: TVector2f): TVector2f;
begin
  Result.X:= Value.X * FTileWidth;
  Result.Y:= Value.Y * FTileHeight;
end;

//------------------------------------------------------------------------------
procedure TPHXTileMap.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXTileMap.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;


{ TPHXRectangularMap }

constructor TPHXRectangularMap.Create;
begin
end;



//------------------------------------------------------------------------------
function TPHXIsometricMap.WorldToScreen(const Value: TVector2f): TVector2f;
begin
  Result.X:= MapOffsetX + (Value.X - Value.Y) * (TileWidth  div 2);
  Result.Y:= MapOffsetY + (Value.X + Value.Y) * (TileHeight div 2);
end;

//------------------------------------------------------------------------------
function TPHXIsometricMap.WorldToScreen(const X, Y : Single): TVector2f;
begin
  Result:= WorldToScreen(Vector2f(X,Y));
end;

//------------------------------------------------------------------------------
function TPHXIsometricMap.ScreenToWorld(const Value: TVector2f): TVector2f;
var X,Y: Single;
begin
  X:= (Value.X - MapOffsetX) / (TileWidth  div 2);
  Y:= (Value.Y - MapOffsetY) / (TileHeight div 2);

  Result.X:= (X + Y) / 2;
  Result.Y:= (Y - X) / 2;
end;

//------------------------------------------------------------------------------
function TPHXIsometricMap.ScreenToWorld(const X, Y : Single): TVector2f;
begin
  Result:= ScreenToWorld(Vector2f(X,Y));
end;









//    Begin at the starting point A and add it to an “open list” of squares to be considered. The open list is kind of like a shopping list. Right now there is just one item on the list, but we will have more later. It contains squares that might fall along the path you want to take, but maybe not. Basically, this is a list of squares that need to be checked out.
//    Look at all the reachable or walkable squares adjacent to the starting point, ignoring squares with walls, water, or other illegal terrain. Add them to the open list, too. For each of these squares, save point A as its “parent square”. This parent square stuff is important when we want to trace our path. It will be explained more later.
//    Drop the starting square A from your open list, and add it to a “closed list” of squares that you don’t need to look at again for now.





{ TPHXPathNode }

constructor TPHXPathNode.Create(const AX, AY: Integer);
begin
  X:= AX;
  Y:= AY;
end;

function TPHXPathNode.ToString: string;
begin
  Result:= IntToStr(Cost);
end;


(*

		/** The x coordinate of the node */
		private int x;
		/** The y coordinate of the node */
		private int y;
		/** The path cost for this node */
		private float cost;
		/** The parent of this node, how we reached it in the search */
		private Node parent;
		/** The heuristic cost of this node */
		private float heuristic;
		/** The search depth of this node */
		private int depth;
*)



// TPHXPathFinder
//------------------------------------------------------------------------------
constructor TPHXPathFinder.Create(AMap: TPHXTileMap);
begin
  FMap := AMap;

  Open:= TList.Create;

  FOpenCount   := 0;
  FOpenCapacity:= 32;


  Init(Map.Layer);
end;

//------------------------------------------------------------------------------
destructor TPHXPathFinder.Destroy;
begin
  ReallocMem(FOpenList, 0);

  Open.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXPathFinder.Init(Layer: TPHXTileLayer);
var Y: Integer;
var X: Integer;
begin
  SetLength(Nodes, Layer.Width, Layer.Height);

  for Y := 0 to Layer.Height - 1 do
  begin
    for X := 0 to Layer.Width - 1 do
    begin
      Nodes[X, Y]:= TPHXPathNode.Create(X, Y);
    end;
  end;

  FOpenCount   := 0;
  FOpenCapacity:= Layer.Width * Layer.Height;

  // In the worst case
  ReallocMem(FOpenList, FOpenCapacity * SizeOf(TPHXPathNode));
end;

//------------------------------------------------------------------------------
procedure TPHXPathFinder.Reset(const TX, TY: Integer);
var Y: Integer;
var X: Integer;
var Node: TPHXPathNode;
begin
  for Y := 0 to Map.Layer.Height - 1 do
  begin
    for X := 0 to Map.Layer.Width - 1 do
    begin
      Node:= Nodes[X, Y];
      Node.Parent   := nil;
      Node.Heuristic:= Abs(X-TX) + Abs(Y-TY);
      Node.Closed   := False;
      Node.Open     := 0;
    end;
  end;
end;





//------------------------------------------------------------------------------
procedure Swap(var A,B: TPHXPathNode);
var T: TPHXPathNode;
begin
  T:= B;
  B:= A;
  A:= T;
end;

//------------------------------------------------------------------------------
function SortNodes(Item1, Item2: Pointer): Integer;
var Node1: TPHXPathNode;
var Node2: TPHXPathNode;
begin
  Node1:= TPHXPathNode(Item1);
  Node2:= TPHXPathNode(Item2);

  Result:= Node2.Cost - Node1.Cost;
end;

(*
  m = numberOfOpenListItems
  While m <> 1 ;While item hasn't bubbled to the top (m=1)

     ;Check if child is <= parent. If so, swap them.
     If Fcost(openList(m)) <= Fcost(openList(m/2)) Then
        temp = openList(m/2)
        openList(m/2) = openList(m)
        openList(m) = temp
        m = m/2
     Else
        Exit ;exit the while/wend loop
     End If
  Wend
*)
// http://www.policyalmanac.org/games/binaryHeaps.htm
//------------------------------------------------------------------------------
procedure TPHXPathFinder.AddToOpen(Node: TPHXPathNode);
//var Index: Integer;
begin
  Inc(FOpenCount);

  Open.Add(Node);

  Open.Sort(SortNodes);

  Inc(Node.Open);
  (*


  Inc(FOpenCount);

  if FOpenCount > FOpenCapacity then
  begin
    FOpenCapacity:= FOpenCount + 32;

    ReallocMem(FOpenList, FOpenCapacity * SizeOf(TPHXPathNode));
  end;

  Index:= FOpenCount;

  FOpenList^[Index]:= Node;
  // While item hasn't bubbled to the top (m=1)
  while Index <> 1 do
  begin
    // Check if child is <= parent. If so, swap them.
    if FOpenList^[Index].Cost < FOpenList^[Index div 2].Cost  then
    begin
      Swap(FOpenList^[Index],  FOpenList^[Index div 2]);

      Index:= Index div 2;
    end else
    begin
      Break;
    end;
  end;
  *)
end;

//------------------------------------------------------------------------------
procedure TPHXPathFinder.RemoveFromCurrent(Node: TPHXPathNode);
//var u,v: Integer;
begin
  Dec(FOpenCount);

  Open.Remove(Node);

  Dec(Node.Open);

(*

  Dec(FOpenCount);

  if FOpenCount > 0 then
  begin
    Swap(FOpenList^[1], FOpenList^[FOpenCount]);

    v:= 1;
    while true do
    begin
      u:= v;
      //if both children exist
      if 2*u+1 <= FOpenCount then
      begin
        // Select the lowest of the two children.
        if FOpenList^[u].Cost >= FOpenList^[2*u  ].Cost then v:= 2*u;
        if FOpenList^[v].Cost >= FOpenList^[2*u+1].Cost then v:= 2*u+1;
      end else
      // if only child #1 exists
      if 2*u <=  FOpenCount then
      begin
        // Check if the F cost is greater than the child
        if FOpenList^[u].Cost >= FOpenList^[2*u].Cost then v:= 2*u;
      end;

      // If parent's F > one or both of its children, swap them
      if u <> v then
      begin
        Swap(FOpenList^[u], FOpenList^[v]);
      end else
      begin
        // if item <= both children, exit repeat/forever loop
        Exit;
      end;
    end;
  end;
 *)

end;

//------------------------------------------------------------------------------
function TPHXPathFinder.InOpenList(Node: TPHXPathNode): Boolean;
begin
//  Result:= Open.IndexOf(Node) <> -1;
  Result:= Node.Open > 0;
  //  not )

end;


{
squaresChecked = squaresChecked +1
  numberOfOpenListItems = numberOfOpenListItems+1
  openList(numberOfOpenListItems) = squaresChecked
}


//------------------------------------------------------------------------------
function TPHXPathFinder.FindPath(const SX, SY, TX, TY: Integer; const Path: TPHXTilePath): Boolean;
var Current  : TPHXPathNode;
var Index    : Integer;
var xp, yp   : Integer;
var Cost     : Integer;
var Neighbour: TPHXPathNode;
var Target   : TPHXPathNode;
begin
  Path.Steps.Clear;

  // easy first check, if the destination is blocked, we can't get there
  if Map.Blocked(tx, ty) then
  begin
    Result:= False;

    Exit;
  end;

  Reset(TX, TY);

  Open.Count:= 0;

  FOpenCount:= 0;

  Nodes[SX, SY].Cost := 0;
  Nodes[SX, SY].Depth:= 0;

  // add the start node
  AddToOpen( Nodes[SX, SY]  );

  // while ((maxDepth < maxSearchDistance) && (open.size() != 0)) {
//  while Open.Count > 0 do
  while FOpenCount > 0 do
  begin
    // pull out the first node in our open list, this is determined to
    // be the most likely to be the next step based on our heuristic
    Current:= Open.List[Open.Count-1];
//    Current:= FOpenList^[1];
    Current.Closed:= True;

    // Remove the node from the open list
    RemoveFromCurrent(Current);

    // search through all the neighbours of the current node evaluating
    // them as next steps
    for Index:= Low(Neighbours) to High(Neighbours) do
    begin
      // determine the location of the neighbour and evaluate it
      xp:= Current.X + Neighbours[Index].DX;
      yp:= Current.Y + Neighbours[Index].DY;

      if (xp >= 0) and (yp >= 0) and (xp < Map.Layer.Width) and (yp < Map.Layer.Height) and not Map.Blocked(xp, yp) then
      begin
        Neighbour:= Nodes[xp][yp];

        Cost:= Current.Cost + Neighbours[Index].Cost;

        // if the new cost we've determined for this node is lower than
        // it has been previously makes sure the node hasn't been discarded. We've
        // determined that there might have been a better path to get to
        // this node so it needs to be re-evaluated
        if (Cost < Neighbour.Cost) then
        begin
          RemoveFromCurrent(Neighbour);

          Neighbour.Closed:= False;
        end;

        // if the node hasn't already been processed and discarded then
        // reset it's cost to our current cost and add it as a next possible
        // step (i.e. to the open list)
        // if (!inOpenList(neighbour) && !(inClosedList(neighbour))) {
        if not InOpenList(Neighbour) and (not Neighbour.Closed) then   //inOpenList(Neighbour)
        begin
          Neighbour.Cost      := Cost;
          Neighbour.Heuristic := abs(xp-tx) + abs(yp-ty);
          Neighbour.Parent    := Current;
          //        maxDepth = Math.max(maxDepth, neighbour.setParent(current));

          AddToOpen(neighbour);
        end;
      end;
    end;
  end;

  // since we've got an empty open list or we've run out of search
  // there was no path. Just return null
  if (Nodes[tx][ty].Parent = nil) then
  begin
    Result:= False;
    Exit;
  end;

  // At this point we've definitely found a path so we can uses the parent
  // references of the nodes to find out way from the target location back
  // to the start recording the nodes on the way.
  Target:= Nodes[tx][ty];
  while (Target <> nodes[sx][sy]) do
  begin
    Path.Steps.Add(target.x, target.y);

    Target:= Target.Parent;
  end;
  Path.Steps.Add(sx, sy);

  Result:= True;
end;





end.
