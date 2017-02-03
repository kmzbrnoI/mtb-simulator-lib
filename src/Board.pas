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
    RG_Failure: TRadioGroup;
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

uses fConfig, LibraryEvents;

{$R *.dfm}

procedure TF_Board.B_ApplyClick(Sender: TObject);
begin
 if (Self.CB_Type.ItemIndex = -1) then
  begin
   MessageBox(F_Board.Handle,'Vyberte typ MTB desky !','Nelze uložit',MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 if ((not Modules[OpenIndex].failure) and (Self.RG_Failure.ItemIndex = 1)) then
  begin
   // module is failing
   Modules[OpenIndex].failure := true;
   if (Assigned(LibEvents.OnError.event)) then
    LibEvents.OnError.event(Self, LibEvents.OnError.data, 141, OpenIndex, 'Modul nekomunikuje');
   if (Assigned(LibEvents.OnOutputChanged.event)) then
    begin
     LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
     LibEvents.OnInputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
    end;
  end;
 if ((not Modules[OpenIndex].failure) and (Self.RG_Exists.ItemIndex = 0)) then
  begin
   // module is restored
   Modules[OpenIndex].failure := false;
   if (Assigned(LibEvents.OnError.event)) then
    LibEvents.OnError.event(Self, LibEvents.OnError.data, 142, OpenIndex, 'Modul komunikuje');
   if (Assigned(LibEvents.OnOutputChanged.event)) then
    begin
     LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
     LibEvents.OnInputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
    end;
  end;

 Modules[OpenIndex].name := Self.E_Name.Text;

 case (Self.CB_Type.ItemIndex) of
  0 : Modules[OpenIndex].typ := idMTB_UNI_ID;
  1 : Modules[OpenIndex].typ := idMTB_UNIOUT_ID;
  2 : Modules[OpenIndex].typ := idMTB_TTL_ID;
  3 : Modules[OpenIndex].typ := idMTB_TTLOUT_ID;
  4 : Modules[OpenIndex].typ := idMTB_REGP_ID;
  5 : Modules[OpenIndex].typ := idMTB_POT_ID;
 end;

 Modules[OpenIndex].fw := Self.E_FW.Text;
 case (Self.RG_Exists.ItemIndex) of
  0:Modules[OpenIndex].exists := false;
  1:Modules[OpenIndex].exists := true;
 end;//case

 Self.Close()
end;//procedure

procedure TF_Board.OpenForm(Module: Integer);
begin
 Self.L_adresa.Caption := IntToStr(Module);
 Self.OpenIndex := Module;
 Self.E_Name.Text := Modules[OpenIndex].name;
 Self.E_FW.Text   := Modules[OpenIndex].fw;

 case (Modules[OpenIndex].exists) of
  false:Self.RG_Exists.ItemIndex := 0;
  true :Self.RG_Exists.ItemIndex := 1;
 end;//case

 case (Modules[OpenIndex].failure) of
  false:Self.RG_Failure.ItemIndex := 0;
  true :Self.RG_Failure.ItemIndex := 1;
 end;//case

 Self.RG_Exists.Enabled := (FormConfig.Status < TSimulatorStatus.starting);
 Self.RG_Failure.Enabled := (FormConfig.Status = TSimulatorStatus.running);

 case (Modules[OpenIndex].typ) of
  idMTB_UNI_ID    : Self.CB_Type.ItemIndex := 0;
  idMTB_UNIOUT_ID : Self.CB_Type.ItemIndex := 1;
  idMTB_TTL_ID    : Self.CB_Type.ItemIndex := 2;
  idMTB_TTLOUT_ID : Self.CB_Type.ItemIndex := 3;
  idMTB_REGP_ID   : Self.CB_Type.ItemIndex := 4;
  idMTB_POT_ID    : Self.CB_Type.ItemIndex := 5;
 end;

 Self.Caption := 'Editovat desku '+IntToStr(Module);
 Self.ShowModal();
end;//procedure

procedure TF_Board.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;//procedure

initialization

finalization
  FreeAndNil(F_Board);

end.//unit
