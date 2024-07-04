unit uEditor;

interface

uses
  System.SysUtils, System.Classes,

  xmldom, XMLIntf, msxmldom, XMLDoc,

  Generics.Collections,

  phxTypes,
  phxGraphics,
  phxDevice,
  phxTexture,

  phxParticle;

type

TPHXParticleEffectEx = class helper for TPHXParticleEffect
  public
    procedure LoadFromXml(const FileName: String);
    procedure SaveToXml(const FileName: String);
  end;

implementation

{ TPHXParticleEffectEx }

procedure TPHXParticleEffectEx.LoadFromXml(const FileName: String);
begin

end;

procedure TPHXParticleEffectEx.SaveToXml(const FileName: String);
begin

end;

end.
