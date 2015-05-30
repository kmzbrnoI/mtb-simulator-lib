////////////////////////////////////////////////////////////////////////////////
//  Board.pas
//  MTB simulator library.
//  MTB board configuration form implemetation.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
// 30.05.2015
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015 Michal Petrilak, Jan Horacek

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
}

{
   DESCRIPTION:

   Configuration form of one MTB board.
}

unit Board;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TF_Board = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CB_Type: TComboBox;
    E_Name: TEdit;
    L_adresa: TLabel;
    B_Apply: TButton;
    B_Storno: TButton;
    Label4: TLabel;
    E_FW: TEdit;
    RG_Exists: TRadioGroup;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private
    OpenIndex:Integer;
  public
    procedure OpenForm(Module:Integer);
  end;

var
  F_Board: TF_Board;

implementation

uses fConfig;

{$R *.dfm}

procedure TF_Board.B_ApplyClick(Sender: TObject);
begin
 if (Self.CB_Type.ItemIndex = -1) then
  begin
   MessageBox(F_Board.Handle,'Vyberte typ MTB desky !','Nelze uložit',MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 if ((Modules[OpenIndex].exists) and (Self.RG_Exists.ItemIndex = 0)) then
  begin
   // module is being non-exist
   if (Assigned(FormConfig.PrgEvents.prgError)) then
    FormConfig.PrgEvents.prgError(Self, 141, OpenIndex, '');
  end;
 if ((not Modules[OpenIndex].exists) and (Self.RG_Exists.ItemIndex = 1)) then
  begin
   // module is being non-exist
   if (Assigned(FormConfig.PrgEvents.prgError)) then
    FormConfig.PrgEvents.prgError(Self, 142, OpenIndex, '');
  end;


 Modules[OpenIndex].name := Self.E_Name.Text;
 Modules[OpenIndex].typ  := Self.CB_Type.Items[Self.CB_Type.ItemIndex];
 Modules[OpenIndex].fw   := Self.E_FW.Text;
 case (Self.RG_Exists.ItemIndex) of
  0:Modules[OpenIndex].exists := false;
  1:Modules[OpenIndex].exists := true;
 end;//case

 Self.Close()
end;//procedure

procedure TF_Board.OpenForm(Module: Integer);
var i:Integer;
begin
 Self.L_adresa.Caption := IntToStr(Module);
 Self.OpenIndex := Module;
 Self.E_Name.Text := Modules[OpenIndex].name;
 Self.E_FW.Text   := Modules[OpenIndex].fw;
 case (Modules[OpenIndex].exists) of
  false:Self.RG_Exists.ItemIndex := 0;
  true :Self.RG_Exists.ItemIndex := 1;
 end;//case

 for i := 0 to Self.CB_Type.Items.Count-1 do
  if (Self.CB_Type.Items.Strings[i] = Modules[OpenIndex].typ) then
   begin
    Self.CB_Type.ItemIndex := i;
    Break;
   end;

 Self.Caption := 'Editovat desku '+IntToStr(Module);
 Self.ShowModal();
end;//procedure

procedure TF_Board.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;//procedure

end.//unit
