unit uImage.List;

interface

uses
 SysUtils, Variants, Classes,
 Graphics, Controls, Forms,Dialogs, StdCtrls,


  phxImage, ExtCtrls, Buttons;

type

  { TFrmImageList }

  TFrmImageList = class(TFrame)
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;

  private

  public
    procedure UpdateImages;
  end;

implementation

{$R *.lfm}

uses uActions;



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
