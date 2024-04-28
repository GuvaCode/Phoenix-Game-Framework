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
unit phxSpriteTerrain;
//< Collision testers for the sprite engine

interface

{$I phxConfig.inc}

uses
  Types, Classes, SysUtils, Math,

  phxLogger,
  phxTypes,

  phxMath,
  phxShape,
  phxSprite,

  phxGraphics,
  phxTexture,

  phxCanvas,
  phxFont,

  UPhysics2D,
  UPhysics2DTypes;

type
TPHXb2Draw = class(Tb2Draw)
  private
    FCanvas: TPHXCanvas;
  public
    constructor Create(ACanvas: TPHXCanvas);

    procedure DrawPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA); override;
    procedure DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSegment(const p1, p2: TVector2; const color: RGBA); override;
    procedure DrawTransform(const xf: Tb2Transform); override;
 end;



TTriangle = record
  p: array[0..2] of TVector2f;

  outerline: array[0..1] of Integer;
end;
TTriangles = array[0..2] of TTriangle;

// The brush is used to draw onto the terrain
//------------------------------------------------------------------------------
TPHXTerrainBrush = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FImage: PSingleList;
    FStrength: Single;
  public
    constructor Create;
    destructor Destroy; override;

    // Resize the brush
    procedure Resize(AWidth, AHeight: Integer);

    // Import the brush
    procedure Import(AWidth, AHeight: Integer; Values: PSingleList);

    // Width of the terrain data
    property Width: Integer read FWidth;
    // Height of the terrain data
    property Height: Integer read FHeight;
    // Pointer to the internal list of values
    property Image: PSingleList read FImage;
    // Strength of the brush
    property Strength: Single read FStrength write FStrength;
  end;

//
//------------------------------------------------------------------------------
TPHXTerrainData = class
  private
    FWidth : Integer;
    FHeight: Integer;
    FList  : PSingleList;

    procedure SetValue(const X, Y: Integer; const Value: Single);
  protected
    function GetRow(const Y: Integer): PSingleList;
    function GetValue(const X,Y: Integer): Single; // inline
  public
    constructor Create;
    destructor Destroy; override;

    // Resize the terrain
    procedure Resize(AWidth, AHeight: Integer);

    // Load the terrain data from a file
    procedure LoadData(const FileName: String); overload;
    // Save the terrain data to a bitmap
    procedure SaveData(const FileName: String);

    // Load the terrain data from a data array
    procedure Import(const Width, Height: Integer; Data: PSingleList); overload;

    procedure Generate(Seed: Cardinal; density_bias: Double; octave_count: Integer = 9; warp_count: Integer = 1);

    // Fill all values
    procedure Fill(const Rect: TRecti; const Value: Single);
    // Blend the the terrain at a rectangle with the values in the data
    procedure Draw(const Rect: TRecti; const Image: PSingleList; const Factor: Single);

    // Generate a texture for this terrain data
    procedure GenerateTexture(const Texture: TPHXTexture; const Color: TColor4f);

    // Width of the terrain data
    property Width: Integer read FWidth;
    // Height of the terrain data
    property Height: Integer read FHeight;
    // Pointer to the internal list of values
    property List: PSingleList read FList;
    // Get and set a value in the data
    property Value[const X,Y: Integer]: Single read GetValue write SetValue; default;
    // Return a pointer to a row
    property Row[const Y: Integer]: PSingleList read GetRow;
  end;


//------------------------------------------------------------------------------
//TPHXTerrainCells = array of TPHXTerrainCell;


//  A cell in the terrain
//
//  2     3
//  +-----+
//  |     |
//  |     |
//  +-----+
//  0     1
//
//------------------------------------------------------------------------------
TPHXTerrainCell = record
  public
    // Index of the cell in the terrain
    Index: TVector2i;
    // Position of the four corners in the cell in world coordinates
    Position: array[0..3] of TVector2f;
    // Values of the four corners in the cell
    Value: array[0..3] of Single;
    // Number of triangles and bodies in the cell
    Count: Integer;
    // The triangles in the cell
    Triangles: TTriangles;
    // The Box2D bodies for each triangle
    Body: array[0..2] of  Tb2Body;
  public
    // Return the center position of the cell
    function GetCenter: TVector2f;
    // Get the cell kind (case)
    function GetKind(const Threshold: Single): Integer;

    // Create a Box2D body for a triangle in this cell
    procedure CreateBody(const World: Tb2World; const Index: Integer);
    // Destroy the Box2D body for a triangle in this cell
    procedure DestroyBody(const World: Tb2World; const Index: Integer);
  end;
PPHXTerrainCell  = ^TPHXTerrainCell;

PCellList = ^TCellList;
TCellList = array[0..$00BFFFFF] of TPHXTerrainCell;

//------------------------------------------------------------------------------
TPHXTerrainCells = class
  private
    FCount: TVector2i;
    FList: PCellList;
  public
    constructor Create;
    destructor Destroy; override;

    // Clear all cells
    procedure Clear;
    // Resize the cells
    procedure Resize(const CountX, CountY: Integer);

    // Number of cells
    property Count: TVector2i read FCount;
    // Number of cells
    property CountX: Integer read FCount.X;
    // Number of cells
    property CountY: Integer read FCount.Y;
    // Pointer to the list of cells
    property List: PCellList read FList;
  end;


//------------------------------------------------------------------------------
TPHXSpriteTerrain = class
  private
    FPhysics: Tb2World;
    FData: TPHXTerrainData;
    FCells: TPHXTerrainCells;
    FScale: TVector2f;
    FThreshold: Single;

    FTexture     : TPHXTexture;
    FTextureScale: TVector2f;

    // Render a triangle for a terrain cell
    procedure RenderTriangle(Canvas: TPHXCanvas; const Triangle: TTriangle);

    function GetCellKind(const Cell: PPHXTerrainCell; const Threshold: Single): Integer;
    function GetCellTriangles(const Cell: PPHXTerrainCell; const Threshold: Single): Integer;

    procedure SetTexture(const Value: TPHXTexture);
  public
    constructor Create(APhysics: Tb2World);
    destructor Destroy; override;

    // Initialize the terrain
    procedure Initialize;

    // Invalidate and regenerate all cells
    procedure Invalidate; overload;
    // Invalidate and regenerate the cells in a rectangle
    procedure Invalidate(const Rect: TRecti); overload;

    // Draw onto the terrain
    procedure Draw(const X,Y: Integer; Brush: TPHXTerrainBrush);

    // Render the terrain
    procedure Render(Canvas: TPHXCanvas);
    // Render the marching squares debug
    procedure RenderDebug(Canvas: TPHXCanvas; Font: TPHXFont);
    // Render all Box2D bodies
    procedure RenderPhysics(Canvas: TPHXCanvas);


    // The physics world
    property Physics: Tb2World read FPhysics;
    // Terrain data
    property Data: TPHXTerrainData read FData;
    // The rhreshold
    property Threshold: Single read FThreshold write FThreshold;
    // The cells in the terrain
    property Cells: TPHXTerrainCells read FCells;
    // Size of each cell in the terrain
    property Scale: TVector2f read FScale write FScale;
    // The texture of the terrain
    property Texture: TPHXTexture read FTexture write SetTexture;
    // The texture scaling of the terrain
    property TextureScale: TVector2f read FTextureScale write FTextureScale;
  end;



implementation

 // TODO: Could unify this with the getPixel function in ImageTracer.h
//       Could pass param for "what happens when coords are outside", although it would be slower.
// Note: This is exactly what a texture-lookup could do in hardware
//------------------------------------------------------------------------------
function sampleBilinearRepeat(Data: PByteArray; const W, H: Integer; u, v: Double): Single;
var x, xn: Integer;
var y, yn: Integer;
var p, np: Double;
var q, nq: Double;
var pxl  : array[0..3] of Byte;
begin
  u:= u * w;
  v:= v * h;
  x:= floor(u);
  y:= floor(v);
  xn:= x+1;
  yn:= y+1;

  p := u - x;
  q := v - y;
  np:= 1 - p;
  nq:= 1 - q;

  x:= x mod w; xn:= xn mod w;
  y:= y mod h; yn:= yn mod h;

  pxl[0]:= Data^[x +y *w];
  pxl[1]:= Data^[xn+y *w];
  pxl[2]:= Data^[x +yn*w];
  pxl[3]:= Data^[xn+yn*w];

  Result:= (pxl[0]*np + pxl[1]*p) * nq + (pxl[2]*np + pxl[3]*p) * q;
end;


 {
// TODO: Could unify this with the getPixel function in ImageTracer.h
//       Could pass param for "what happens when coords are outside", although it would be slower.
// Note: This is exactly what a texture-lookup could do in hardware
function sampleBilinearRepeat(const uchar image[],
                                   int width, int height,
                                   uchar channel_count,
                                   double u, double v,    // Texture-space coordinates
                                   uchar channel=0)       // Colour channel
begin
    // For brevity
    int w = width;
    int h = height;
    u *= w;
    v *= h;
    int x = (int)floor(u);
    int y = (int)floor(v);
    int xn = x+1;
    int yn = y+1;
    double p = u - x;
    double q = v - y;
    double np = 1-p;
    double nq = 1-q;
    x %= w; xn %= w;
    y %= h; yn %= h;
    uchar pxl[4] = begin
        image[(x+y*w)   * channel_count + channel],
        image[(xn+y*w)  * channel_count + channel],
        image[(x+yn*w)  * channel_count + channel],
        image[(xn+yn*w) * channel_count + channel]
    end;
    return (pxl[0]*np + pxl[1]*p) * nq +
           (pxl[2]*np + pxl[3]*p) * q;
end

// Generates a random terrain (i.e., density/height map) image
int genTerrain(uchar out_image[],
               int width, int height,
               uchar colour_count,          // TODO: Necessary?
               unsigned int seed=0,         // Random seed
               double density_bias=0.0,     // Starting density
               int octave_count=9,          // Number of noise octaves to sum
               double octave_freqs[]=NULL,  // Octave frequencies
               double octave_ampls[]=NULL,  // Octave amplitudes
               int warp_count=1,            // Number of noise octaves to warp coords with
               double warp_freqs[]=NULL,    // Warp octave frequencies
               double warp_amples[]=NULL)   // Warp octave amplitudes
begin
    // Build a bunch of 2D random-noise lookup-tables
    // ----------------------------------------------
    static const int noise_channels = 9;
    static const int noise_x_samples = 32;
    static const int noise_y_samples = 32;
    static const int noise_samples = noise_x_samples*
                                     noise_y_samples;
    // Seed the random number generator
    srand(seed);
    uchar* noise[noise_channels];
    for( int channel=0; channel!=noise_channels; ++channel ) begin
        noise[channel] = (uchar*)malloc(noise_x_samples*
                                        noise_y_samples*
                                        1);
        for( int sample=0; sample!=noise_samples; ++sample )
            // Crude but easy random sample
            noise[channel][sample] = rand() % 256;
    end
    // ----------------------------------------------

    // Generate the terrain
    double* density = new double[width*height];
    double max_density = -1e99;
    double min_density = 1e99;
    for( int y=0; y!=height; ++y ) begin
        for( int x=0; x!=width; ++x ) begin
            // Get the u, v coordinates
            double u = (double)x / (double)width;
            double v = (double)y / (double)height;

            // Shortcut
            double& d = density[x+y*width];

            // Set initial density
            d = density_bias;

            // Apply radial density function (e.g., for creating planets)
            //d -= 10.0 * (0.1 - sqrt( (u-0.5)*(u-0.5)+(v-0.5)*(v-0.5) ));

            // Apply warping of the coordinates
            for( int i=0; i!=warp_count; ++i ) begin
                u += (sampleBilinearRepeat(
                          noise[i%noise_channels],
                          noise_x_samples, noise_y_samples,
                          1,
                          u * warp_freqs[i]/noise_x_samples,
                          v * warp_freqs[i]/noise_y_samples
                       ) / 128.0 - 1.0) * warp_amples[i];
                u += (sampleBilinearRepeat(
                          noise[(i+1)%noise_channels],
                          noise_x_samples, noise_y_samples,
                          1,
                          u * warp_freqs[i],
                          v * warp_freqs[i]
                       ) / 128.0 - 1.0) * warp_amples[i];
            end

            // Add octaves of noise
            for( int i=0; i!=octave_count; ++i ) begin
                d += (sampleBilinearRepeat(
                         noise[i%noise_channels],
                         noise_x_samples, noise_y_samples,
                         1,
                         u * octave_freqs[i]/noise_x_samples,
                         v * octave_freqs[i]/noise_y_samples
                      ) / 128.0 - 1.0) * octave_ampls[i];
            end

            // Could apply other density functions...
            // e.g., User-defined "flat spots", terracing,
            // maze generator, errosion models, anything!

            // Remember the max and min density values within
            // the terrain.
            if( d > max_density )
                max_density = d;
            else if( d < min_density )
                min_density = d;
        end
    end

    // Free the allocated noise sample memory
    for( int channel=0; channel!=noise_channels; ++channel )
        free(noise[channel]);

    //cout << "Min density: " << min_density << endl;
    //cout << "Max density: " << max_density << endl;

    //double density_range = max_density - min_density;
    double density_range = MAX(max_density, -min_density);

    // Write the new density image
    // Re-map the range of density values to colour values
    // and write to all three colour channels.
    for( int i=0; i!=width*height; ++i ) begin
        // Set density in all colour channels
        for( int c=0; c!=colour_count; ++c )
            out_image[i*colour_count + c] = (uchar)((density[i] / density_range + 1) * 127);
    end

    delete [] density;

    // Success
    return 0;
end
*)

  }

//------------------------------------------------------------------------------
function Interpolate(const Cell: PPHXTerrainCell; const Threshold: Single; const pid1, pid2: Integer): TVector2f;
var P1, P2: TVector2f;
var valp1: Single;
var valp2: Single;
var m: Single;
begin
  p1:= Cell.Position[pid1];
  p2:= Cell.Position[pid2];

  valp1:= Cell.Value[pid1];
  valp2:= Cell.Value[pid2];


  if Abs(Threshold - valp1) < 0.00001 then
  begin
    Result:= p1;
  end else
  if Abs(Threshold - valp2) < 0.00001 then
  begin
    Result:= P2;
  end else
  if Abs(valp1 - valp2) < 0.00001 then
  begin
    Result:= P1;
  end else
  begin
    m:= (Threshold - valp1) / (valp2 - valp1);

    Result.x:= p1.x + m * (p2.x - p1.x);
    Result.y:= p1.y + m * (p2.y - p1.y);
  end;
end;

{$REGION 'TPHXTerrainBrush'}

// TPHXTerrainBrush
//==============================================================================
constructor TPHXTerrainBrush.Create;
begin
  FWidth   := 0;
  FHeight  := 0;
  FImage   := nil;
  FStrength:= 0.10;
end;

//------------------------------------------------------------------------------
destructor TPHXTerrainBrush.Destroy;
begin
  ReallocMem(FImage, 0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainBrush.Resize(AWidth, AHeight: Integer);
begin
  FWidth:= AWidth;
  FHeight:= AHeight;

  ReallocMem(FImage, (AWidth *  AHeight) * SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainBrush.Import(AWidth, AHeight: Integer; Values: PSingleList);
begin
  Resize(AWidth, AHeight);

  System.Move(Values^, FImage^, AWidth * AHeight * SizeOf(Single));
end;


{$REGION 'TPHXTerrainData'}

// TPHXTerrainData
//==============================================================================
constructor TPHXTerrainData.Create;
begin
  FWidth := 0;
  FHeight:= 0;
  FList  := nil;

end;

//------------------------------------------------------------------------------
destructor TPHXTerrainData.Destroy;
begin
  ReallocMem(FList, 0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.Resize(AWidth, AHeight: Integer);
begin
  FWidth:= AWidth;
  FHeight:= AHeight;

  ReallocMem(FList, (AWidth *  AHeight) * SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.LoadData(const FileName: String);
var Bitmap: TPHXBitmap;
var X, Y: Integer;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.LoadBitmap(FileName);

    Resize(Bitmap.Width, Bitmap.Height);

    for Y:=0 to FHeight - 1 do
    begin
      for X:=0 to FWidth - 1 do
      begin
        FList^[X + Y * Width]:=  Bitmap.Pixel[X,Y].Red / 255;
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.Import(const Width, Height: Integer; Data: PSingleList);
var X, Y: Integer;
begin
  Resize(Width, Height);

  for Y:=0 to Height - 1 do
  begin
    for X:=0 to Width - 1 do
    begin
      FList^[X + Y * Width]:= Data^[X + Y * Width];
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.SaveData(const FileName: String);
var Bitmap: TPHXBitmap;
var X, Y  : Integer;
var Pixel : TPHXPixel;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.Resize(Width, Height, pfRGB);

    for Y:=0 to FHeight - 1 do
    begin
      for X:=0 to FWidth - 1 do
      begin
        Pixel.Red  := Round(FList^[X + Y * Width] * 255);
        Pixel.Green:= Round(FList^[X + Y * Width] * 255);
        Pixel.Blue := Round(FList^[X + Y * Width] * 255);
        Pixel.Alpha:= 255;

        Bitmap.Pixel[X,Y]:= Pixel;
      end;
    end;
    Bitmap.SaveBitmap(FileName);
  finally
    Bitmap.Free;
  end;
end;

// Build a bunch of 2D random-noise lookup-tables
// ----------------------------------------------
const noise_channels = 9;
const noise_x_samples = 32;
const noise_y_samples = 32;
const noise_samples = noise_x_samples * noise_y_samples;
{
int genTerrain(uchar out_image[],
               int width, int height,
               uchar colour_count,          // TODO: Necessary?
               unsigned int seed=0,         // Random seed
               double density_bias=0.0,     // Starting density
               int octave_count=9,          // Number of noise octaves to sum
               double octave_freqs[]=NULL,  // Octave frequencies
               double octave_ampls[]=NULL,  // Octave amplitudes
               int warp_count=1,            // Number of noise octaves to warp coords with
               double warp_freqs[]=NULL,    // Warp octave frequencies
               double warp_amples[]=NULL)   // Warp octave amplitudes

}

// Octave frequencies
const octave_freqs: array[0..8] of Double = (10, 10, 10, 10, 10, 10, 10, 10, 10);
// Octave amplitudes
const octave_ampls: array[0..8] of Double = (0.25, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50);

//------------------------------------------------------------------------------
procedure TPHXTerrainData.Generate(Seed: Cardinal; density_bias: Double; octave_count: Integer = 9; warp_count: Integer = 1);
var Noise: array[0..noise_channels-1, 0..noise_samples-1] of Byte;
var Channel: Integer;
var Sample: Integer;
var Density: array of Double;
var DensityMax: Double;
var DensityMin: Double;
var DensityRange: Double;
var x,y: Integer;
var v,u: Double;
var d  : PDouble;
var i  : Integer;
begin
  // Seed the random number generator
  RandSeed:= Seed;

  for Channel := 0 to noise_channels - 1 do
  begin
    for Sample := 0 to noise_samples - 1 do
    begin
      noise[Channel, Sample]:= Random(255);
    end;
  end;

  DensityMax:= -1e99;
  DensityMin:= 1e99;

  SetLength(Density, Width* Height);


  for Y:=0 to Height - 1 do
  begin
    for X:=0 to Width - 1 do
    begin
      // Get the u, v coordinates
      u:= x / width;
      v:= y / height;

      // Shortcut
      d:= @density[x+y*width];

      // Set initial density
      d^:= density_bias;

      // Apply radial density function (e.g., for creating planets)
      //d -= 10.0 * (0.1 - sqrt( (u-0.5)*(u-0.5)+(v-0.5)*(v-0.5) ));
         {
      // Apply warping of the coordinates
      for I:=0 to warp_count - 1 do
      begin
        u:= u + (sampleBilinearRepeat(
                  noise[i%noise_channels],
                  noise_x_samples, noise_y_samples,
                  1,
                  u * warp_freqs[i]/noise_x_samples,
                  v * warp_freqs[i]/noise_y_samples
               ) / 128.0 - 1.0) * warp_amples[i];
        u += (sampleBilinearRepeat(
                  noise[(i+1)%noise_channels],
                  noise_x_samples, noise_y_samples,
                  1,
                  u * warp_freqs[i],
                  v * warp_freqs[i]
               ) / 128.0 - 1.0) * warp_amples[i];
      end  }

      // Add octaves of noise
      for I:=0 to octave_count - 1 do
      begin
        d^:= d^ + (sampleBilinearRepeat(@noise[i mod noise_channels],
                                        noise_x_samples,
                                        noise_y_samples,
                                        u * octave_freqs[i]/noise_x_samples,
                                        v * octave_freqs[i]/noise_y_samples
                                        ) / 128.0 - 1.0) * octave_ampls[i];

      end;
      // Could apply other density functions...
      // e.g., User-defined "flat spots", terracing,
      // maze generator, errosion models, anything!

      // Remember the max and min density values within
      // the terrain.
      if( d^ > DensityMax ) then
      begin
        DensityMax:= d^;
      end else
      if( d^ < DensityMin ) then
      begin
        DensityMin:= d^;
      end;
    end;
  end;

  //cout << "Min density: " << min_density << endl;
  //cout << "Max density: " << max_density << endl;

  //double density_range = max_density - min_density;
  DensityRange:= MAX(DensityMax, -DensityMin);

  // Write the new density image
  // Re-map the range of density values to colour values
  // and write to all three colour channels.
  for I:=0 to Width * Height - 1 do
  begin
    FList^[I]:= ((density[i] / DensityRange + 1) * 0.5);

  end;
end;


//------------------------------------------------------------------------------
procedure TPHXTerrainData.Fill(const Rect: TRecti; const Value: Single);
var X, Y: Integer;
begin
  for Y:= Rect.Top to Rect.Bottom - 1 do
  begin
    for X:= Rect.Left to Rect.Right - 1 do
    begin
      FList^[X + Y * FWidth]:= Value;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.Draw(const Rect: TRecti; const Image: PSingleList; const Factor: Single);
var X,Y: Integer;
var W,H: Integer;

var ISrc, IDst: Integer;
begin
  W:= (Rect.Right  - Rect.Left);
  H:= (Rect.Bottom - Rect.Top);

  ISrc:= 0;
  for Y:= 0 to H - 1 do
  begin
    IDst:= Rect.Left + (Rect.Top + Y) * FWidth;
    for X:= 0 to W - 1 do
    begin

      if (Rect.Left + X >= 0) and (Rect.Left + X < FWidth) and (Rect.Top + Y >= 0) and (Rect.Top + Y < FHeight) then
      begin
        FList^[IDst]:=  Max(0, FList^[IDst] - Image^[ISrc] * Factor);
      end;

      Inc(IDst);
      Inc(ISrc);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.GenerateTexture(const Texture: TPHXTexture; const Color: TColor4f);
var X,Y,I: Integer;
var Line : pByte;
begin
  Texture.Resize(FWidth, FHeight, pfRGB);

  I:= 0;
  for Y:=0 to FHeight - 1 do
  begin
    Line:= Texture.ScanLine(Y);

    for X:=0 to FWidth - 1 do
    begin
      PColor3b(Line).Red  := Round(Color.Red   * FList^[I] * 255);
      PColor3b(Line).Green:= Round(Color.Green * FList^[I] * 255);
      PColor3b(Line).Blue := Round(Color.Blue  * FList^[I] * 255);

      Inc(I);

      Inc(Line, 3);
    end;
  end;

  Texture.Build;
end;

//------------------------------------------------------------------------------
function TPHXTerrainData.GetRow(const Y: Integer): PSingleList;
begin
  Result:= @FList^[Y * Width];
end;

//------------------------------------------------------------------------------
function TPHXTerrainData.GetValue(const X, Y: Integer): Single;
begin
  if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
  begin
    Result:= FList^[X + Y * Width];
  end else
  begin
    // external_colour
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainData.SetValue(const X, Y: Integer; const Value: Single);
begin
  if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
  begin
    FList^[X + Y * Width]:= Value;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXTerrainCell'}

// TPHXTerrainCell
//==============================================================================
function TPHXTerrainCell.GetCenter: TVector2f;
begin
  Result.X:= (Position[1].X - Position[0].X) * 0.5;
  Result.Y:= (Position[2].Y - Position[0].Y) * 0.5;
end;

//------------------------------------------------------------------------------
function TPHXTerrainCell.GetKind(const Threshold: Single): Integer;
const BoolValue: array[Boolean] of Integer = (0, 1);
begin
  Result:= 1 * BoolValue[Value[0] >= Threshold] +
           2 * BoolValue[Value[1] >= Threshold] +
           8 * BoolValue[Value[2] >= Threshold] +
           4 * BoolValue[Value[3] >= Threshold];
end;


//------------------------------------------------------------------------------
procedure TPHXTerrainCell.CreateBody(const World: Tb2World; const Index: Integer);
var Shape  : Tb2PolygonShape;
var BodyDef: Tb2BodyDef;
var FixtureDef    : Tb2FixtureDef;
var Vertices: array[0..2] of TVector2;
begin
  // TODO: Offset to cell center
  Vertices[0].x:= Triangles[Index].p[0].X * (1 / 32);
  Vertices[0].y:= Triangles[Index].p[0].Y * (1 / 32);

  Vertices[1].x:= Triangles[Index].p[1].X * (1 / 32);
  Vertices[1].y:= Triangles[Index].p[1].Y * (1 / 32);

  Vertices[2].x:= Triangles[Index].p[2].X * (1 / 32);
  Vertices[2].y:= Triangles[Index].p[2].Y * (1 / 32);

  Shape:= Tb2PolygonShape.Create;
  Shape.SetVertices(@Vertices[0], 3);

  bodyDef := Tb2BodyDef.Create;
  bodyDef.bodyType := b2_staticBody;
  bodyDef.position.X:= 0;//Position.X * TPHXPhysicsEngine(Engine).FScaleInv;
  bodyDef.position.Y:= 0;//Position.Y * TPHXPhysicsEngine(Engine).FScaleInv;


  FixtureDef := Tb2FixtureDef.Create;
  FixtureDef.shape      := Shape;
  FixtureDef.density    := 0;
  FixtureDef.friction   := 0.2;
  FixtureDef.restitution:= 0;

  Body[Index]:= World.CreateBody(bodyDef);
  // Add the shape to the body.
  Body[Index].CreateFixture(FixtureDef);
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainCell.DestroyBody(const World: Tb2World; const Index: Integer);
begin
  World.DestroyBody(Body[Index], True);

  Body[Index]:= nil;
end;

{$ENDREGION}

{$REGION 'TPHXTerrainCells'}

// TPHXTerrainCells
//==============================================================================
constructor TPHXTerrainCells.Create;
begin
  FCount.X:= 0;
  FCount.Y:= 0;
  FList   := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXTerrainCells.Destroy;
begin
  Clear;

  ReallocMem(FList, 0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainCells.Clear;
var I,J: Integer;
begin
  // Free all Box2D bodies
  for I := 0 to (FCount.X * FCount.Y) - 1 do
  begin
    for J := 0 to FList^[I].Count - 1 do
    begin
      if Assigned(FList^[I].Body[J]) then
      begin
        /// TODO: Must remove from the Box2D world
        FreeAndNil(FList^[I].Body[J]);
      end;
    end;
  end;
  FCount.X:= 0;
  FCount.Y:= 0;

  ReallocMem(FList, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXTerrainCells.Resize(const CountX, CountY: Integer);
begin
  if (FCount.X <> CountX) or (FCount.Y <> CountY) then
  begin
    Clear;

    FCount.X:= CountX;
    FCount.Y:= CountY;

    ReallocMem(FList, (FCount.X * FCount.Y) * SizeOf(TPHXTerrainCell));

   // FillChar(FList^, (FCount.X * FCount.Y) * SizeOf(TPHXTerrainCell), #0);
  end;
end;

{$ENDREGION}

// TPHXSpriteTerrain
//==============================================================================
constructor TPHXSpriteTerrain.Create(APhysics: Tb2World);
begin
  FPhysics:= APhysics;
  FData   := TPHXTerrainData.Create;
  FCells  := TPHXTerrainCells.Create;

  FThreshold:= 0.14;

  FScale.X:= 32;
  FScale.Y:= 32;

  FTexture       := nil;
  FTextureScale.X:= 1.0;
  FTextureScale.Y:= 1.0;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteTerrain.Destroy;
begin
  FData.Free;
  FCells.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.Initialize;
var X,Y,I: Integer;
var Cell : PPHXTerrainCell;
var Index: Integer;
begin
  Cells.Resize(Data.Width-1, Data.Height - 1);
  (*
    // Index of the cell in the terrain
    Index: TVector2i;
    // Position of the four corners in the cell in world coordinates
    Position: array[0..3] of TVector2f;
    // Values of the four corners in the cell
    Value: array[0..3] of Single;
    // Number of triangles and bodies in the cell
    Count: Integer;
    // The triangles in the cell
    Triangle: array[0..2] of TTriangle;
    // The Box2D bodies for each triangle
    Body: array[0..2] of  Tb2Body;
    *)

  I:= 0;
  // Generate the cell data
  for Y:=0 to Cells.CountY - 1 do
  begin
    for X:=0 to Cells.CountX - 1 do
    begin
      Cell:= @Cells.List^[I];
      Cell.Index.X:= X;
      Cell.Index.Y:= Y;

      Cell.Position[0]:= Vector2f(X * FScale.X          ,  Y * FScale.Y);
      Cell.Position[1]:= Vector2f(X * FScale.X + FScale.X, Y * FScale.Y);
      Cell.Position[2]:= Vector2f(X * FScale.X           , Y * FScale.Y + FScale.Y);
      Cell.Position[3]:= Vector2f(X * FScale.X + FScale.X, Y * FScale.Y + FScale.Y);

      Cell.Value[0]:= Data.GetValue(X  ,   Y  );
      Cell.Value[1]:= Data.GetValue(X + 1, Y  );
      Cell.Value[2]:= Data.GetValue(X    , Y+1);
      Cell.Value[3]:= Data.GetValue(X + 1, Y+1);

      Cell.Body[0]:= nil;
      Cell.Body[1]:= nil;
      Cell.Body[2]:= nil;

      Cell.Count:= GetCellTriangles(Cell, Threshold);

      // Create Box2D bodies
      for Index := 0 to Cell.Count - 1 do
      begin
        Cell.CreateBody(FPhysics, Index);
      end;

      Inc(I);
    end;
  end;

//  Invalidate(Recti(0,0, Cells.CountX-1, Cells.CountY-1));
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.Invalidate;
begin
  Invalidate(Recti(0,0, Cells.CountX-1, Cells.CountY-1));
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.Invalidate(const Rect: TRecti);
var ARect: TRect;
var X,Y,I: Integer;
var Index: Integer;
var Cell : PPHXTerrainCell;
begin
  ARect.Left  := Max(Rect.Left  , 0);
  ARect.Right := Min(Rect.Right , Cells.CountX - 1);
  ARect.Top   := Max(Rect.Top   , 0);
  ARect.Bottom:= Min(Rect.Bottom, Cells.CountY - 1);

  for Y:= ARect.Top to ARect.Bottom do
  begin
    I:= ARect.Left + (Y * Cells.CountX);
    for X:= ARect.Left to ARect.Right do
    begin
      Cell:= @Cells.List^[I];

      // Free box2D bodies
      for Index := 0 to Cell.Count - 1 do
      begin
        if Assigned(Cell.Body[Index]) then
        begin
          Cell.DestroyBody(FPhysics, Index);
        end;
      end;

      Cell.Value[0]:= Data.List^[(X  ) + (Y  ) * Data.Width];// .GetValue(X  ,   Y  );
      Cell.Value[1]:= Data.List^[(X+1) + (Y  ) * Data.Width];//Data.GetValue(X + 1, Y  );
      Cell.Value[2]:= Data.List^[(X  ) + (Y+1) * Data.Width];//Data.GetValue(X    , Y+1);
      Cell.Value[3]:= Data.List^[(X+1) + (Y+1) * Data.Width];//Data.GetValue(X + 1, Y+1);

      Cell.Count:= GetCellTriangles(Cell, Threshold);

      // Create Box2D bodies
      for Index := 0 to Cell.Count - 1 do
      begin
        Cell.CreateBody(FPhysics, Index);
      end;

      Inc(I);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.Draw(const X, Y: Integer; Brush: TPHXTerrainBrush);
var Rect: TRecti;
begin
  Rect.Left  := X;
  Rect.Top   := Y;
  Rect.Right := X + Brush.Width;
  Rect.Bottom:= Y + Brush.Height;

//  Data.Draw(X,Y, Brush.Width, Brush.Height, Brush.Image, Brush.Strength);
  Data.Draw(Rect, Brush.Image, Brush.Strength);

  Invalidate(Rect);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.Render(Canvas: TPHXCanvas);
var X,Y,I: Integer;
var Count: Integer;
var Index: Integer;
begin
  Canvas.Texture:= Texture;
  Canvas.Primitive:= PHX_TRIANGLES;

  I:= 0;
  for Y:=0 to Cells.CountY - 1 do
  begin
    for X:=0 to Cells.CountX - 1 do
    begin
      Count:= Cells.List^[I].Count;

      for Index := 0 to Count - 1 do
      begin
        RenderTriangle(Canvas, Cells.List^[I].Triangles[Index]);
        //Canvas.FilledTriangle(Triangles[Index].p[0], Triangles[Index].p[1], Triangles[Index].p[2]);
      end;

      Inc(I);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.RenderDebug(Canvas: TPHXCanvas; Font: TPHXFont);
var X,Y,I: Integer;
var Cell : PPHXTerrainCell;
var Kind: Integer;
var Count: Integer;
var Index: Integer;
var Triangle: TTriangle;
begin
  I:= 0;
  for Y:=0 to Cells.CountY - 1 do
  begin
    for X:=0 to Cells.CountX - 1 do
    begin
      Cell:= @Cells.List^[I];

      Kind:= GetCellKind(Cell, Threshold);

      Font.TextOut(Cell.Position[0].x, Cell.Position[0].y, IntToStr(Kind), clrSilver);

      Inc(I);
    end;
  end;

  Canvas.Texture:= nil;
  Canvas.Color:= clrSilver;

  I:= 0;
  for Y:=0 to Cells.CountY - 1 do
  begin
    for X:=0 to Cells.CountX - 1 do
    begin
      Count:= Cells.List^[I].Count;// GetCellTriangles(Cells.List^[I], Threshold, Triangles);

      for Index := 0 to Count - 1 do
      begin
        Triangle:= Cells.List^[I].Triangles[Index];

        Canvas.Triangle(Triangle.p[0], Triangle.p[1], Triangle.p[2]);
      end;

      Inc(I);
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.RenderPhysics(Canvas: TPHXCanvas);
var Draw: TPHXb2Draw;
begin
  Canvas.Texture:= nil;
  Canvas.Color  := clrSilver;

  Draw:= TPHXb2Draw.Create(Canvas);
  try
    Draw.m_drawFlags:= [e_shapeBit];

    Physics.Draw:= Draw;
    Physics.DrawDebugData;
    Physics.Draw:= nil;
  finally
    Draw.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.RenderTriangle(Canvas: TPHXCanvas; const Triangle: TTriangle);
var VI: Integer;
var II: Integer;
var C: TColor4f;
begin
  if Canvas.FlushRequired(3, 3) then
  begin
    Canvas.Flush;
  end;

  //c.Red:= Cell.v[0];
  //c.Green:= Cell.v[0];
  //c.Blue:= Cell.v[0];
  //c.Alpha:= 1;

  c:= clrWhite;

  VI:= Canvas.Vertices.Alloc(3);

  // Vertex #1
  Canvas.Vertices.List^[VI+0].TexCoord.X:= Triangle.P[0].X * FTextureScale.X;
  Canvas.Vertices.List^[VI+0].TexCoord.Y:= Triangle.P[0].Y * FTextureScale.Y;

  Canvas.Vertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  Canvas.Vertices.List^[VI+0].Position.X:= Triangle.P[0].X;
  Canvas.Vertices.List^[VI+0].Position.Y:= Triangle.P[0].Y;
  Canvas.Vertices.List^[VI+0].Position.Z:= 0;

  Canvas.Vertices.List^[VI+0].Color:= c;

  // Vertex #2
  Canvas.Vertices.List^[VI+1].TexCoord.X:= Triangle.P[1].X * FTextureScale.X;
  Canvas.Vertices.List^[VI+1].TexCoord.Y:= Triangle.P[1].Y * FTextureScale.Y;

  Canvas.Vertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  Canvas.Vertices.List^[VI+1].Position.X:= Triangle.P[1].X;
  Canvas.Vertices.List^[VI+1].Position.Y:= Triangle.P[1].Y;
  Canvas.Vertices.List^[VI+1].Position.Z:= 0;
  Canvas.Vertices.List^[VI+1].Color:= c;


  // Vertex #3
  Canvas.Vertices.List^[VI+2].TexCoord.X:= Triangle.P[2].X * FTextureScale.X;
  Canvas.Vertices.List^[VI+2].TexCoord.Y:= Triangle.P[2].Y * FTextureScale.Y;

  Canvas.Vertices.List^[VI+2].Normal:= Vector3f_AxisZ;

  Canvas.Vertices.List^[VI+2].Position.X:= Triangle.P[2].X;
  Canvas.Vertices.List^[VI+2].Position.Y:= Triangle.P[2].Y;
  Canvas.Vertices.List^[VI+2].Position.Z:= 0;

  Canvas.Vertices.List^[VI+2].Color:= c;

  II:= Canvas.Indices.Alloc(3);

  // Triangle #1
  Canvas.Indices.List^[II+0]:= VI+0;
  Canvas.Indices.List^[II+1]:= VI+1;
  Canvas.Indices.List^[II+2]:= VI+2;
end;


{$REGION 'Calculations'}

// 8   4
// +---+
// |   |
// +---+
// 1   2
//------------------------------------------------------------------------------
function TPHXSpriteTerrain.GetCellKind(const Cell: PPHXTerrainCell; const Threshold: Single): Integer;
const Value: array[Boolean] of Integer = (0, 1);
begin
  Result:= 1 * Value[Cell.Value[0] >= Threshold] +
           2 * Value[Cell.Value[1] >= Threshold] +
           8 * Value[Cell.Value[2] >= Threshold] +
           4 * Value[Cell.Value[3] >= Threshold];
end;

// All cases
//
// Case 0   Case 1   Case 2   Case 3   Case 4   Case 5   Case 6   Case 7
// O-----O  O-----O  O-----O  O-----O  O-----#  O-----#  O-----#  O-----#
// |     |  |     |  |     |  |     |  |    \|  |    \|  |  |  |  |/    |
// |     |  |\    |  |    /|  |-----|  |     |  |\    |  |  |  |  |     |
// O-----O  #-----O  O-----#  #-----#  O-----O  #-----O  O-----#  #-----#
//
// Case 8   Case 9   Case 10  Case 11  Case 12  Case 13  Case 14  Case 15
// #-----O  #-----O  #-----O  #-----O  #-----#  #-----#  #-----#  #-----#
// |/    |  |  |  |  |/    |  |    \|  |-----|  |     |  |     |  |     |
// |     |  |  |  |  |    /|  |     |  |     |  |    /|  |\    |  |     |
// O-----O  #-----O  O-----#  #-----#  O-----O  #-----O  O-----#  #-----#
//------------------------------------------------------------------------------
function TPHXSpriteTerrain.GetCellTriangles(const Cell: PPHXTerrainCell; const Threshold: Single): Integer;
var Kind: Integer;
begin
  Kind:= GetCellKind(Cell, Threshold);

	Result:=0;

  case Kind of
    0:
    begin
      Result:= 0;
    end;
    1:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 2, 0);
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[0].p[2]:= Cell.Position[0];
      Cell.Triangles[0].outerline[0]:= 0;
      Cell.Triangles[0].outerline[1]:= 1;

      Result:= 1;
    end;
    2:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[0].p[2]:= Cell.Position[1];

      Cell.Triangles[0].outerline[0]:= 0;
      Cell.Triangles[0].outerline[1]:= 1;

      Result:= 1;
    end;
    3:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[0].p[1]:= Cell.Position[1];
      Cell.Triangles[0].p[2]:= Cell.Position[0];
      // no outer line...
      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[1].p[2]:= Cell.Position[1];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Result:= 2;
    end;
    4:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[2]:= Cell.Position[3];
      Cell.Triangles[0].outerline[0]:= 0;
      Cell.Triangles[0].outerline[1]:= 1;

      Result:= 1;
    end;
    5:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[2]:= Cell.Position[3];
      Cell.Triangles[0].outerline[0]:= 0;
      Cell.Triangles[0].outerline[1]:= 1;

      Cell.Triangles[1].p[0]:= Cell.Position[0];
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[1].p[2]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].outerline[0]:= 1;
      Cell.Triangles[1].outerline[1]:= 2;

      Result:= 2;
    end;
    6:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[1]:= Cell.Position[3];
      Cell.Triangles[0].p[2]:= Cell.Position[1];
      // no outer line...

      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[1].p[2]:= Cell.Position[1];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Result:= 2;
    end;
    7:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[1]:= Cell.Position[3];
      Cell.Triangles[0].p[2]:= Cell.Position[1];
      // no outer line...

      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[1].p[2]:= Cell.Position[1];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Cell.Triangles[2].p[0]:= Cell.Position[0];
      Cell.Triangles[2].p[1]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[2].p[2]:= Cell.Position[1];
      // no outer line...

      Result:= 3;
    end;
    8:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[0].p[2]:= Cell.Position[2];
      Cell.Triangles[0].outerline[0]:= 0;
      Cell.Triangles[0].outerline[1]:= 1;

      Result:= 1;
    end;
    9:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[0];
      Cell.Triangles[0].p[1]:= Cell.Position[2];
      Cell.Triangles[0].p[2]:= Interpolate(Cell, Threshold, 0, 1);
      // no outer line...

      Cell.Triangles[1].p[0]:= Cell.Position[2];
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[1].p[2]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].outerline[0]:= 1;
      Cell.Triangles[1].outerline[1]:= 2;

      Result:= 2;
    end;
    10:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[2];
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[0].p[2]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[0].outerline[0]:= 1;
      Cell.Triangles[0].outerline[1]:= 2;

      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[1].p[2]:= Cell.Position[1];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Result:= 2;
    end;
    11:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[0];
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[0].p[2]:= Cell.Position[1];
      // no outer line...

      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[1].p[2]:= Cell.Position[0];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Cell.Triangles[2].p[0]:= Cell.Position[2];
      Cell.Triangles[2].p[1]:= Interpolate(Cell, Threshold, 2, 3);
      Cell.Triangles[2].p[2]:= Cell.Position[0];
      // no outer line...

      Result:= 3;
    end;
    12:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[2];
      Cell.Triangles[0].p[1]:= Cell.Position[3];
      Cell.Triangles[0].p[2]:= Interpolate(Cell, Threshold, 0, 2);
      // no outer line...

      Cell.Triangles[1].p[0]:= Cell.Position[3];
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[1].p[2]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[1].outerline[0]:= 1;
      Cell.Triangles[1].outerline[1]:= 2;

      Result:= 2;
    end;
    13:
    begin
      Cell.Triangles[0].p[0]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[0].p[1]:= Cell.Position[0];
      Cell.Triangles[0].p[2]:= Cell.Position[2];
      // no outer line...

      Cell.Triangles[1].p[0]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].p[2]:= Cell.Position[2];
      Cell.Triangles[1].outerline[0]:= 0;
      Cell.Triangles[1].outerline[1]:= 1;

      Cell.Triangles[2].p[0]:= Interpolate(Cell, Threshold, 1, 3);
      Cell.Triangles[2].p[1]:= Cell.Position[2];
      Cell.Triangles[2].p[2]:= Cell.Position[3];
      // no outer line...

      Result:= 3;
    end;
    14:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[1];
      Cell.Triangles[0].p[1]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[0].p[2]:= Cell.Position[3];
      // no outer line...

      Cell.Triangles[1].p[0]:= Cell.Position[3];
      Cell.Triangles[1].p[1]:= Interpolate(Cell, Threshold, 0, 1);
      Cell.Triangles[1].p[2]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[1].outerline[0]:= 1;
      Cell.Triangles[1].outerline[1]:= 2;

      Cell.Triangles[2].p[0]:= Interpolate(Cell, Threshold, 0, 2);
      Cell.Triangles[2].p[1]:= Cell.Position[2];
      Cell.Triangles[2].p[2]:= Cell.Position[3];
      // no outer line...

      Result:= 3;
    end;
    15:
    begin
      Cell.Triangles[0].p[0]:= Cell.Position[2];
      Cell.Triangles[0].p[1]:= Cell.Position[1];
      Cell.Triangles[0].p[2]:= Cell.Position[0];
      // no outer line...

      Cell.Triangles[1].p[0]:= Cell.Position[1];
      Cell.Triangles[1].p[1]:= Cell.Position[2];
      Cell.Triangles[1].p[2]:= Cell.Position[3];
      // no outer line...

      Result:= 2;
    end;
  end;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TPHXSpriteTerrain.SetTexture(const Value: TPHXTexture);
begin
  FTexture := Value;

  if Assigned(FTexture) and (FTexture.Width > 0) and (FTexture.Height > 0) then
  begin
    FTextureScale.X:= 1 / (Data.Width  * FScale.X) * ((Data.Width  * FScale.X) / Texture.Width );
    FTextureScale.Y:= 1 / (Data.Height * FScale.Y) * ((Data.Height * FScale.Y) / Texture.Height);
  end else
  begin
    FTextureScale.X:= 1.0;
    FTextureScale.Y:= 1.0;
  end;
end;



















(*

   Tb2DrawBits = (e_shapeBit, e_jointBit, e_aabbBit, e_pairBit,
      e_centerOfMassBit{$IFDEF CONTROLLERS}, e_controllerBit{$ENDIF});
   Tb2DrawBitsSet = set of Tb2DrawBits;

   Tb2Draw = class
   public
      m_drawFlags: Tb2DrawBitsSet;
      m_shapeColor_Inactive, m_shapeColor_Static, m_shapeColor_Kinematic,
      m_shapeColor_Sleeping, m_shapeColor_Normal,
      m_pairColor, m_aabbColor, m_world_aabbColor, m_coreColor, m_jointLineColor: RGBA;
      *)
{ TPHXb2Draw }

constructor TPHXb2Draw.Create(ACanvas: TPHXCanvas);
begin
  FCanvas:= ACanvas;
end;

procedure TPHXb2Draw.DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA);
begin
  FCanvas.Circle(Center.x, Center.y, Radius);
end;

procedure TPHXb2Draw.DrawPolygon(const vertices: Tb2PolyVertices;vertexCount: Int32; const color: RGBA);
begin

end;

procedure TPHXb2Draw.DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA);
begin

end;

procedure TPHXb2Draw.DrawSegment(const p1, p2: TVector2; const color: RGBA);
begin

end;

procedure TPHXb2Draw.DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA);
begin
  FCanvas.Circle(Center.x * 32, Center.y * 32, Radius * 32);
end;

procedure TPHXb2Draw.DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA);
var Index: Integer;
var P1   : TVector2f;
var P2   : TVector2f;
begin
  if vertexCount = 0 then Exit;

  P1.X:= vertices[vertexCount-1].x * 32;
  P1.Y:= vertices[vertexCount-1].Y * 32;

  for Index := 0 to vertexCount-1 do
  begin
    P2.X:= vertices[Index].x * 32;
    P2.Y:= vertices[Index].Y * 32;

    FCanvas.Line(P1, P2);

    P1:= P2;
  end;
end;

procedure TPHXb2Draw.DrawTransform(const xf: Tb2Transform);
begin

end;



end.
