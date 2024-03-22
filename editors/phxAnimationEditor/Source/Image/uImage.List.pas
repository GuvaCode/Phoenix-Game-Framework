unit uImage.List;

interface

uses
 SysUtils, Variants, Classes,
 Graphics, Controls, Forms,Dialogs, StdCtrls,


  phxImage, ExtCtrls;

type

  { TFrmImageList }

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

{$R *.lfm}

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

{ TFrmImageList }



end.
