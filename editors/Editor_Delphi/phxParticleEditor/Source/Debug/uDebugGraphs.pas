unit uDebugGraphs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,

  phxParticle,
  phxParticleGraphs;

type
  TFrmDebugGraphs = class(TForm)
    sgGraph: TStringGrid;
  private
    FEffect: TPHXParticleEffect;

    procedure sgGraphUpdate;

    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;

var
  FrmDebugGraphs: TFrmDebugGraphs;

implementation

{$R *.dfm}

{ TFrmDebugGraphs }

procedure TFrmDebugGraphs.SetEffect(const Value: TPHXParticleEffect);
begin
  FEffect := Value;

  sgGraphUpdate;
end;

procedure TFrmDebugGraphs.sgGraphUpdate;
var Index: Integer;
begin
  sgGraph.RowCount:= 1 + Effect.Graphs.Alpha.Count;

  sgGraph.Cells[1,0]:= 'Alpha';
  for Index := 0 to Effect.Graphs.Alpha.Count-1 do
  begin
    sgGraph.Cells[0,Index+1]:= IntToStr(Index);
    sgGraph.Cells[1,Index+1]:= FloatToStr(Effect.Graphs.Alpha.Values^[Index]);
  end;

end;

end.
