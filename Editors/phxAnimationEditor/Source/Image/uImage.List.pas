unit uImage.List;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,


  phxImage, Vcl.ExtCtrls;

type
  TFrmImageList = class(TFrame)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
  private
    { Private declarations }
  public
    procedure UpdateImages;
  end;

implementation

{$R *.dfm}

uses uActions;

{ TFrmImageList }

procedure TFrmImageList.UpdateImages;
var Image: TPHXImage;
begin
  ListBox1.Items.Clear;
  for Image in ModActions.Images do
  begin
      ListBox1.Items.Add(Image.Name);

  end;
end;

end.
