program Texture2Bytes;

{$APPTYPE CONSOLE}

uses SysUtils, // ialogs,//SysUtils, Types, Classes, ,

  phxGraphics,
  phxGraphics_FreeImage;

var Format: TPHXGraphicFormat;
var Graphic: TPHXGraphic;

var Source: string;
var DestName: string;
var DestFile: TextFile;
var X,Y: Integer;
var Pixel: PCardinal;
var PixelSize: Integer;
begin
  Source  := ParamStr(1);
  DestName:= ParamStr(2);

  if not FileExists(Source) then
  begin
    WriteLn('Could not open source file: "' + Source + '".');

    Exit;
  end;

  if DestName = '' then
  begin
    DestName:= ChangeFileExt(Source, '.inc');
  end;

  if GraphicFormats.Find(Source, Format) then
  begin
    Format.Filer.LoadFromFile(Source, Graphic);

    PixelSize:= GetPixelFormatSize(Graphic.Format);

    Assign(DestFile, DestName);
    try
      Rewrite(DestFile);

      Writeln(DestFile, '// this file was automatically generated from "'+ Source + '"');
      Writeln(DestFile, 'array [0 .. ', Graphic.Width * Graphic.Height - 1, '] of Cardinal = (');

      for Y := 0 to Graphic.Height-1 do
      begin
        for X := 0 to Graphic.Width-1 do
        begin
          Pixel:= @Graphic.Pixels[(X + Y * Graphic.Width) * PixelSize];

          Write(DestFile, '$', IntToHex(Pixel^, 8));

          if (X+1<Graphic.Width) or (Y+1<Graphic.Height) then
          begin
            Write(DestFile, ',');
          end;
        end;
        WriteLn(DestFile);
      end;
      Writeln(DestFile, ');');
    finally
      CloseFile(DestFile);
    end;
    end;

end.
