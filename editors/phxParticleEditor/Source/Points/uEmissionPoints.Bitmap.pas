unit uEmissionPoints.Bitmap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, JvToolEdit, StdCtrls, Mask, JvExMask, JvSpin,

  phxParticle;

type
  TFrmEmittorTemplateBitmap = class(TFrame)
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label3: TLabel;
    cbDirection: TComboBox;
    edInterval: TJvSpinEdit;
    Label4: TLabel;
    edFilename: TJvComboEdit;
    procedure edFilenameButtonClick(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
  private
    function GetFilename: String;
  public
    procedure Apply(Effect: TPHXParticleEffect; Count: Integer);

    property Filename: String read GetFilename;
  end;

implementation


{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFrmEmittorTemplateBitmap.edFilenameButtonClick(Sender: TObject);
var FileExt: String;
begin
  if OpenDialog1.Execute then
  begin
    FileExt:= ExtractFileExt(OpenDialog1.FileName);

    if SameText(FileExt, '.bmp') then
    begin
       edFilename.Text:= OpenDialog1.FileName;
    end;
  end;
//  btnImport.Enabled:= FileExists(FileName);
end;

//------------------------------------------------------------------------------
procedure TFrmEmittorTemplateBitmap.edFilenameChange(Sender: TObject);
begin
 // btnImport.Enabled:= FileExists(FileName);
end;

//------------------------------------------------------------------------------
procedure TFrmEmittorTemplateBitmap.Apply(Effect: TPHXParticleEffect; Count: Integer);
var Bitmap: TBitmap;
var Interval: Integer;
var X: Integer;
var Y: Integer;
var I: Integer;
begin
  Interval:= Round(edInterval.Value);

  if not FileExists(FileName) then Exit;
  

  Bitmap:= TBitmap.Create;
  try
    Bitmap.LoadFromFile(FileName);

    Effect.Emittor.Points.Clear;

    I:=0;
    case cbDirection.ItemIndex of
      //Horisontal, Left to right
      0:
      begin
        for X := 0 to Bitmap.Width - 1 do
          for Y := 0 to Bitmap.Height - 1 do
          begin
            if (I = 0) and (Bitmap.Canvas.Pixels[X,Y] <> clWhite) then Effect.Emittor.Points.Add(X,Y, 0);

            I:= (I + 1) mod Interval;
          end;
      end;
      1:
      //Horisontal, Right to left
      begin
        for X := Bitmap.Width - 1 downto 0 do
          for Y := 0 to Bitmap.Height - 1 do
          begin
            if (I = 0) and (Bitmap.Canvas.Pixels[X,Y] <> clWhite) then Effect.Emittor.Points.Add(X,Y, 0);

            I:= (I + 1) mod Interval;
          end;
      end;
      //Vertical, Top to bottom
      2:
      begin
        for Y := 0 to Bitmap.Height - 1 do
          for X := 0 to Bitmap.Width - 1 do
          begin
            if (I = 0) and (Bitmap.Canvas.Pixels[X,Y] <> clWhite) then Effect.Emittor.Points.Add(X,Y, 0);

            I:= (I + 1) mod Interval;
          end;
      end;
      //Vertical, Bottom to top
      3:
      begin
        for Y := Bitmap.Height - 1 downto 0 do
          for X := 0 to Bitmap.Width - 1 do
          begin
            if (I = 0) and (Bitmap.Canvas.Pixels[X,Y] <> clWhite) then Effect.Emittor.Points.Add(X,Y, 0);

            I:= (I + 1) mod Interval;
          end;
      end;
    end;

  finally
    Bitmap.Free;
  end;
  //PaintBox1.Invalidate;
end;




//------------------------------------------------------------------------------
function TFrmEmittorTemplateBitmap.GetFilename: String;
begin
  Result:= edFilename.Text;
end;

end.
