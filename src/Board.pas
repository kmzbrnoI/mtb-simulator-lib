{
  MTB board configuration form implemetation.

  LICENSE:

  Copyright 2015-2021 Michal Petrilak, Jan Horacek

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

unit Board;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Generics.Collections;

type
  TF_Board = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    E_Name: TEdit;
    L_adresa: TLabel;
    B_Apply: TButton;
    B_Storno: TButton;
    Label4: TLabel;
    E_FW: TEdit;
    GB_IO_type: TGroupBox;
    CHB_IR0: TCheckBox;
    CHB_IR3: TCheckBox;
    CHB_IR2: TCheckBox;
    CHB_IR1: TCheckBox;
    E_Type: TEdit;
    CHB_IR6: TCheckBox;
    CHB_IR5: TCheckBox;
    CHB_IR4: TCheckBox;
    CHB_IR7: TCheckBox;
    CHB_IR15: TCheckBox;
    CHB_IR14: TCheckBox;
    CHB_IR13: TCheckBox;
    CHB_IR12: TCheckBox;
    CHB_IR11: TCheckBox;
    CHB_IR10: TCheckBox;
    CHB_IR9: TCheckBox;
    CHB_IR8: TCheckBox;
    CHB_SCOM0: TCheckBox;
    CHB_SCOM1: TCheckBox;
    CHB_SCOM2: TCheckBox;
    CHB_SCOM3: TCheckBox;
    CHB_SCOM4: TCheckBox;
    CHB_SCOM5: TCheckBox;
    CHB_SCOM7: TCheckBox;
    CHB_SCOM6: TCheckBox;
    CHB_SCOM8: TCheckBox;
    CHB_SCOM9: TCheckBox;
    CHB_SCOM10: TCheckBox;
    CHB_SCOM11: TCheckBox;
    CHB_SCOM12: TCheckBox;
    CHB_SCOM13: TCheckBox;
    CHB_SCOM14: TCheckBox;
    CHB_SCOM15: TCheckBox;
    CHB_Exists: TCheckBox;
    CHB_failure: TCheckBox;
    CHB_Error: TCheckBox;
    CHB_Warning: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

    chb_irs: TList<TCheckBox>;
    chb_scoms: TList<TCheckBox>;

  public
    OpenIndex: Integer;

    procedure OpenForm(Module: Integer);
  end;

var
  F_Board: TF_Board;

implementation

uses fConfig, LibraryEvents, Errors;

{$R *.dfm}

procedure TF_Board.FormCreate(Sender: TObject);
begin
  Self.chb_irs := TList<TCheckBox>.Create();
  Self.chb_irs.Add(Self.CHB_IR0);
  Self.chb_irs.Add(Self.CHB_IR1);
  Self.chb_irs.Add(Self.CHB_IR2);
  Self.chb_irs.Add(Self.CHB_IR3);
  Self.chb_irs.Add(Self.CHB_IR4);
  Self.chb_irs.Add(Self.CHB_IR5);
  Self.chb_irs.Add(Self.CHB_IR6);
  Self.chb_irs.Add(Self.CHB_IR7);
  Self.chb_irs.Add(Self.CHB_IR8);
  Self.chb_irs.Add(Self.CHB_IR9);
  Self.chb_irs.Add(Self.CHB_IR10);
  Self.chb_irs.Add(Self.CHB_IR11);
  Self.chb_irs.Add(Self.CHB_IR12);
  Self.chb_irs.Add(Self.CHB_IR13);
  Self.chb_irs.Add(Self.CHB_IR14);
  Self.chb_irs.Add(Self.CHB_IR15);

  Self.chb_scoms := TList<TCheckBox>.Create();
  Self.chb_scoms.Add(Self.CHB_SCOM0);
  Self.chb_scoms.Add(Self.CHB_SCOM1);
  Self.chb_scoms.Add(Self.CHB_SCOM2);
  Self.chb_scoms.Add(Self.CHB_SCOM3);
  Self.chb_scoms.Add(Self.CHB_SCOM4);
  Self.chb_scoms.Add(Self.CHB_SCOM5);
  Self.chb_scoms.Add(Self.CHB_SCOM6);
  Self.chb_scoms.Add(Self.CHB_SCOM7);
  Self.chb_scoms.Add(Self.CHB_SCOM8);
  Self.chb_scoms.Add(Self.CHB_SCOM9);
  Self.chb_scoms.Add(Self.CHB_SCOM10);
  Self.chb_scoms.Add(Self.CHB_SCOM11);
  Self.chb_scoms.Add(Self.CHB_SCOM12);
  Self.chb_scoms.Add(Self.CHB_SCOM13);
  Self.chb_scoms.Add(Self.CHB_SCOM14);
  Self.chb_scoms.Add(Self.CHB_SCOM15);
end;

procedure TF_Board.FormDestroy(Sender: TObject);
begin
  Self.chb_irs.Free();
  Self.chb_scoms.Free();
end;

procedure TF_Board.B_ApplyClick(Sender: TObject);
begin
  if ((not modules[OpenIndex].failure) and (Self.CHB_failure.Checked)) then
  begin
    // module is failing
    modules[OpenIndex].failure := true;
    if (Assigned(LibEvents.OnError.event)) then
      LibEvents.OnError.event(Self, LibEvents.OnError.data, RCS_MODULE_FAILED, OpenIndex, 'Modul nekomunikuje');
    if (Assigned(LibEvents.OnOutputChanged.event)) then
      LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
    if (Assigned(LibEvents.OnInputChanged.event)) then
      LibEvents.OnInputChanged.event(FormConfig, LibEvents.OnInputChanged.data, OpenIndex);
    if (Assigned(LibEvents.OnModuleChanged.event)) then
      LibEvents.OnModuleChanged.event(FormConfig, LibEvents.OnModuleChanged.data, OpenIndex);
  end;
  if (((modules[OpenIndex].failure) and (not Self.CHB_failure.Checked)) or
    ((FormConfig.Status = TSimulatorStatus.running) and (not modules[OpenIndex].exists) and
    (Self.CHB_Exists.Checked))) then
  begin
    // module is restored
    modules[OpenIndex].failure := false;
    if (Assigned(LibEvents.OnError.event)) then
      LibEvents.OnError.event(Self, LibEvents.OnError.data, RCS_MODULE_RESTORED, OpenIndex, 'Modul komunikuje');
    if (Assigned(LibEvents.OnOutputChanged.event)) then
      LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, OpenIndex);
    if (Assigned(LibEvents.OnInputChanged.event)) then
      LibEvents.OnInputChanged.event(FormConfig, LibEvents.OnInputChanged.data, OpenIndex);
    if (Assigned(LibEvents.OnModuleChanged.event)) then
      LibEvents.OnModuleChanged.event(FormConfig, LibEvents.OnModuleChanged.data, OpenIndex);
  end;

  modules[OpenIndex].name := Self.E_Name.Text;
  modules[OpenIndex].typ := Self.E_Type.Text;
  modules[OpenIndex].fw := Self.E_FW.Text;

  modules[OpenIndex].exists := Self.CHB_Exists.Checked;

  var errorChanged: Boolean := (modules[OpenIndex].error <> Self.CHB_Error.Checked);
  modules[OpenIndex].error := Self.CHB_Error.Checked;

  var warningChanged: Boolean := (modules[OpenIndex].warning <> Self.CHB_Warning.Checked);
  modules[OpenIndex].warning := Self.CHB_Warning.Checked;

  modules[OpenIndex].irs := 0;
  for var i : Integer := 0 to Self.chb_irs.Count-1 do
    if (Self.chb_irs[i].Checked) then
      modules[OpenIndex].irs := modules[OpenIndex].irs or (1 shl i);

  modules[OpenIndex].scoms := 0;
  for var i : Integer := 0 to Self.chb_scoms.Count-1 do
    if (Self.chb_scoms[i].Checked) then
      modules[OpenIndex].scoms := modules[OpenIndex].scoms or (1 shl i);

  FormConfig.SaveData(FormConfig.config_fn);

  if ((errorChanged) or (warningChanged)) then
    if (Assigned(LibEvents.OnModuleChanged.event)) then
      LibEvents.OnModuleChanged.event(FormConfig, LibEvents.OnModuleChanged.data, OpenIndex);
end;

procedure TF_Board.OpenForm(Module: Integer);
begin
  Self.L_adresa.Caption := IntToStr(Module);
  Self.OpenIndex := Module;
  Self.E_Name.Text := modules[OpenIndex].name;
  Self.E_FW.Text := modules[OpenIndex].fw;

  Self.CHB_Exists.Checked := modules[OpenIndex].exists;
  Self.CHB_failure.Checked := modules[OpenIndex].failure;
  Self.CHB_Error.Checked := modules[OpenIndex].error;
  Self.CHB_Warning.Checked := modules[OpenIndex].warning;

  Self.E_Type.Text := modules[OpenIndex].typ;

  begin
    var irs := modules[OpenIndex].irs;
    for var i : Integer := 0 to Self.chb_irs.Count-1 do
    begin
      Self.chb_irs[i].Checked := ((irs and 1) > 0);
      irs := irs shr 1;
    end;
  end;

  begin
    var scoms := modules[OpenIndex].scoms;
    for var i : Integer := 0 to Self.chb_scoms.Count-1 do
    begin
      Self.chb_scoms[i].Checked := ((scoms and 1) > 0);
      scoms := scoms shr 1;
    end;
  end;

  Self.Caption := 'Upravit desku ' + IntToStr(Module);
  Self.ActiveControl := Self.E_Name;
  Self.Show();
end;

procedure TF_Board.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

initialization

finalization

FreeAndNil(F_Board);

end.// unit
