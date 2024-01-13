unit uModel;

interface

uses SysUtils, Classes,

 // Xml.XmlDoc, Xml.XmlIntf,

  phxClasses,
  phxModel;

type

TPHXModelEx = class helper for TPHXMesh
  public
    // Load the model from a stream
    procedure LoadModel(Reader: TPHXReader);
    // Save the model to a stream
    procedure SaveModel(Writer: TPHXWriter);

    procedure LoadFromXml(const FileName: String);
    procedure SaveToXml(const FileName: String);
  end;


implementation

{ TPHXModelEx }

procedure TPHXModelEx.LoadModel(Reader: TPHXReader);
begin
  inherited LoadModel(Reader);
end;

procedure TPHXModelEx.SaveModel(Writer: TPHXWriter);
begin
  inherited SaveModel(Writer);
end;


procedure TPHXModelEx.LoadFromXml(const FileName: String);
begin

end;



procedure TPHXModelEx.SaveToXml(const FileName: String);
begin

end;

end.
