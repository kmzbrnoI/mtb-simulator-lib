////////////////////////////////////////////////////////////////////////////////
//  Board.pas
//  MTB simulator library.
//  MTB board configuration form implemetation.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2017 Michal Petrilak, Jan Horacek

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
    GB_IO_type: TGroupBox;
    chb_IR0: TCheckBox;
    chb_IR3: TCheckBox;
    chb_IR2: TCheckBox;
    chb_IR1: TCheckBox;
    chb_SCOM0: TCheckBox;
    chb_SCOM1: TCheckBox;
    chb_SCOM2: TCheckBox;
    chb_SCOM3: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private

  public
    OpenIndex:Integer;

    procedure OpenForm(Module:Integer);
  end;

var
  F_Board: TF_Board;

implementation

uses fConfig, LibraryEvents, Errors;

{$R *.dfm}

procedure TF_Board.B_ApplyClick(Sender: TObject);
begin
 if (Self.CB_Type.ItemIndex = -1) then
  begin
   MessageBox(F_Board.Handle,'Vyberte typ MTB desky!','Nelze uložit',MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 if ((not Modules[OpenIndex].failure) and (Self.RG_Failure.ItemIndex = 1)) then
  begin
   // module is failing
   Modules[OpenIndex].failure := true;
   if (Assigned(LibEvents.OnError.event)) then
    LibEvents.OnError.event(Self, LibEvents.OnError.data, RCS_MODULE_FAILED, OpenIndex, 'Modul nekomunikuje');
   if (Assigned(LibEvents.OnOutputChanged.event)) then
    begin
     LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
     LibEvents.OnInputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
    end;
  end;
 if (((Modules[OpenIndex].failure) and (Self.RG_Failure.ItemIndex = 0)) or
     ((FormConfig.Status = TSimulatorStatus.running) and (not Modules[OpenIndex].exists) and (Self.RG_Exists.ItemIndex = 1))) then
  begin
   // module is restored
   Modules[OpenIndex].failure := false;
   if (Assigned(LibEvents.OnError.event)) then
    LibEvents.OnError.event(Self, LibEvents.OnError.data, RCS_MODULE_RESTORED, OpenIndex, 'Modul komunikuje');
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

 Modules[OpenIndex].ir := 0;
 if (Self.chb_IR0.Checked) then Modules[OpenIndex].ir := Modules[OpenIndex].ir or 1;
 if (Self.chb_IR1.Checked) then Modules[OpenIndex].ir := Modules[OpenIndex].ir or 2;
 if (Self.chb_IR2.Checked) then Modules[OpenIndex].ir := Modules[OpenIndex].ir or 4;
 if (Self.chb_IR3.Checked) then Modules[OpenIndex].ir := Modules[OpenIndex].ir or 8;

 Modules[OpenIndex].scom := 0;
 if (Self.chb_SCOM0.Checked) then Modules[OpenIndex].scom := Modules[OpenIndex].scom or 1;
 if (Self.chb_SCOM1.Checked) then Modules[OpenIndex].scom := Modules[OpenIndex].scom or 2;
 if (Self.chb_SCOM2.Checked) then Modules[OpenIndex].scom := Modules[OpenIndex].scom or 4;
 if (Self.chb_SCOM3.Checked) then Modules[OpenIndex].scom := Modules[OpenIndex].scom or 8;

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

 Self.RG_Exists.Enabled := (FormConfig.Status <> TSimulatorStatus.running) or (not Modules[OpenIndex].exists);
 Self.RG_Failure.Enabled := (FormConfig.Status = TSimulatorStatus.running) and (Modules[OpenIndex].exists);
 Self.GB_IO_type.Enabled := (FormConfig.Status <> TSimulatorStatus.running);

 case (Modules[OpenIndex].typ) of
  idMTB_UNI_ID    : Self.CB_Type.ItemIndex := 0;
  idMTB_UNIOUT_ID : Self.CB_Type.ItemIndex := 1;
  idMTB_TTL_ID    : Self.CB_Type.ItemIndex := 2;
  idMTB_TTLOUT_ID : Self.CB_Type.ItemIndex := 3;
  idMTB_REGP_ID   : Self.CB_Type.ItemIndex := 4;
  idMTB_POT_ID    : Self.CB_Type.ItemIndex := 5;
 end;

 Self.chb_IR0.Checked := Boolean(Modules[OpenIndex].ir and $1);
 Self.chb_IR1.Checked := Boolean((Modules[OpenIndex].ir shr 1) and $1);
 Self.chb_IR2.Checked := Boolean((Modules[OpenIndex].ir shr 2) and $1);
 Self.chb_IR3.Checked := Boolean((Modules[OpenIndex].ir shr 3) and $1);

 Self.chb_SCOM0.Checked := Boolean(Modules[OpenIndex].scom and $1);
 Self.chb_SCOM1.Checked := Boolean((Modules[OpenIndex].scom shr 1) and $1);
 Self.chb_SCOM2.Checked := Boolean((Modules[OpenIndex].scom shr 2) and $1);
 Self.chb_SCOM3.Checked := Boolean((Modules[OpenIndex].scom shr 3) and $1);

 Self.Caption := 'Editovat desku '+IntToStr(Module);
 Self.Show();
end;//procedure

procedure TF_Board.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;//procedure

initialization

finalization
  FreeAndNil(F_Board);

end.//unit
