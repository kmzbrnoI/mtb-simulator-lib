////////////////////////////////////////////////////////////////////////////////
//  fConfig.pas
//  MTB simulator library.
//  Main configuration form implemetation.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2018 Michal Petrilak, Jan Horacek

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

   Main configuration form. See Simulator.dpr comments to uderstand how to
   interact with it.
}

unit fConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IniFiles, Menus, StdCtrls, Spin;

const
  _DEFAULT_CFG_FILE = 'rcs/simcfg.ini';        // Config file. Change MTB ranges to make form more synoptic.
  _MAX_MTB = 255;

type
  TMyEvent  = function(Sender:TObject):Integer of object; stdcall;
  TMyErrorEvent = function (Sender: TObject; errValue: word; errAddr: Cardinal; errStr:string):Integer of object; stdcall;

  TModulType = (idNone = $0, idMTB_POT_ID = $10, idMTB_REGP_ID = $30, idMTB_UNI_ID = $40,
        idMTB_UNIOUT_ID = $50, idMTB_TTL_ID = $60, idMTB_TTLOUT_ID = $70);

  // Simulation status:
  TSimulatorStatus = (closed = 0, opening = 1, closing = 2, stopped = 3, starting = 4, running = 5, stopping = 6);

  TRCSIPortType = (
    iptPlain = 0,
    iptIR = 1
  );

  TRCSOPortType = (
    optPlain = 0,
    optSCom = 1
  );

  // One MTB module
  TModule = record
    name:string;
    typ:TModulType;
    fw:string;
    exists:boolean;
    failure:boolean;
    ir:Cardinal;
    scom:Cardinal;
  end;

  // Form
  TFormConfig = class(TForm)
    GB_Error: TGroupBox;
    Label1: TLabel;
    SE_Err_id: TSpinEdit;
    Label2: TLabel;
    SE_Err_board: TSpinEdit;
    B_Error: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_ErrorClick(Sender: TObject);
    procedure SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
  private
    Cfgbtn:array[0.._MAX_MTB] of TButton;                        // configuration buttons
    pin: Array[0.._MAX_MTB, 0..16] of TShape;                    // pins (= ports)

    fstatus : TSimulatorStatus;                                  // simulation status

    procedure ChangeInput(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowAddress(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CfgBtnOnClick(Sender:TObject);
    procedure CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure SetStatus(new:TSimulatorStatus);
  public

   pins_start:Cardinal;
   pins_end:Cardinal;
   config_fn:string;

    procedure RePaintPins();

    procedure LoadData(filename:string);
    procedure SaveData(filename:string);
    procedure CreatePins();
    procedure FreePins();

    procedure OnOpen(Sender:TObject);
    procedure OnClose(Sender:TObject);
    procedure OnStart(Sender:TObject);
    procedure OnStop(Sender:TObject);

    procedure OnScanned(Sender:TObject);

    property Status:TSimulatorStatus read fstatus write SetStatus;
  end;

var
  FormConfig: TFormConfig;

var
  vstup: Array[0.._MAX_MTB, 0..15] of Byte;                  // input states
  vystup: Array[0.._MAX_MTB, 0..15] of Byte;                 // output states
  Modules:array[0.._MAX_MTB] of TModule;                     // modules config
  api_version:Cardinal;


implementation

uses Board, LibraryEvents;

{$R *.dfm}

procedure TFormConfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := false;
  Hide;
end;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
 Self.config_fn := _DEFAULT_CFG_FILE;
 try
   Self.LoadData(Self.config_fn);
 except

 end;

 Self.CreatePins();
 Self.status := TSimulatorStatus.closed;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
 try
   Self.SaveData(Self.config_fn);
   Self.FreePins();
 except

 end;
end;//procedure

procedure TFormConfig.CreatePins();
var i, j:Cardinal;
begin
 for i := Self.pins_start to Self.pins_end do
  begin
   Cfgbtn[i] := TButton.Create(FormConfig);
   with (Cfgbtn[i]) do
    begin
     Parent  := FormConfig;
     Top     := 5;
     Left    := (i-Self.pins_start)*15 + 5;
     Caption := '?';
     Height  := 25;
     Width   := 13;
     Tag     := i;
     OnClick := Self.CfgBtnOnClick;
     OnMouseMove := Self.CfgBtnOnMove;
    end;
   for j := 0 to 15 do
    begin
     pin[i, j] := TShape.Create(FormConfig);
     with pin[i, j] do
      begin
       Parent := FormConfig;
       Left := (i-Self.pins_start)*15 + 5;
       Top := j*15 + 35;
       Width := 13;
       Height := 13;
       Pen.Width := 2;
       Tag := 16*i + j;
       OnMouseUp := ChangeInput;
       OnMouseMove := ShowAddress;
      end;
    end;
  end;
 Self.RePaintPins();

 Self.ClientWidth := (((Self.pins_end-Self.pins_start)+1)*(15))+8;
 Self.ClientHeight := (16*15)+40 + GB_Error.Height + 2;
end;

procedure TFormConfig.FreePins();
var i, j:Integer;
begin
 for i := 0 to _MAX_MTB do
  begin
   if (Assigned(Self.Cfgbtn[i])) then FreeAndNil(Self.Cfgbtn[i]);
   for j := 0 to 15 do
     if (Assigned(Self.pin[i, j])) then FreeAndNil(Self.pin[i, j]);
  end;
end;

procedure TFormConfig.ChangeInput(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Module, Port: Integer;
begin
  Module := (Sender as TShape).Tag div 16;
  Port   := (Sender as TShape).Tag mod 16;

  vstup[Module, Port] := vstup[Module, Port] XOR 1;
  RePaintPins();
  if (Assigned(LibEvents.OnInputChanged.event)) then LibEvents.OnInputChanged.event(Self, LibEvents.OnInputChanged.data, Module);
end;

procedure TFormConfig.RePaintPins();
var
  i, j: integer;
  sh: TShape;
begin
  for i := Self.pins_start to Self.pins_end do begin
    for j := 0 to 15 do begin
      sh := pin[i, j];

      if ((Modules[i].ir shr (j div 4)) and $1 > 0) then
        sh.Pen.Color := clFuchsia * vstup[i, j]
      else
        sh.Pen.Color := clRed * vstup[i, j];

      if ((Modules[i].scom shr (j div 2)) and $1 > 0) then begin
        sh.Brush.Color := clAqua * Integer(vystup[i, j] > 0);
        sh.Hint := IntToStr(vystup[i, j]);
      end else
        sh.Brush.Color := clLime * Integer(vystup[i, j] > 0);
    end;
  end;
end;

procedure TFormConfig.ShowAddress(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Module, Port: Integer;
begin
  Module := (Sender as TShape).Tag div 16;
  Port   := (Sender as TShape).Tag mod 16;

  Caption := Format('%.3d / %.3d', [Module, Port])
end;

procedure TFormConfig.LoadData(filename:string);
var Ini:TMemIniFile;
    i:Integer;
begin
 Ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   Self.pins_start := ini.ReadInteger('MTB', 'start', 0);
   Self.pins_end   := ini.ReadInteger('MTB', 'end', 31);

   if (Self.pins_end < Self.pins_start) then
    Self.pins_end := Self.pins_start;

   for i := 0 to _MAX_MTB do
    begin
     Modules[i].name   := Ini.ReadString('MTB'+IntToStr(i),'name','Simulator'+IntToStr(i));
     Modules[i].typ    := TModulType(Ini.ReadInteger('MTB'+IntToStr(i),'typ', Integer(idMTB_UNI_ID)));
     Modules[i].fw     := Ini.ReadString('MTB'+IntToStr(i),'fw','VIRTUAL');
     Modules[i].exists := Ini.ReadBool('MTB'+IntToStr(i),'is',true);
     Modules[i].ir     := Ini.ReadInteger('MTB'+IntToStr(i),'ir',0);
     Modules[i].scom   := Ini.ReadInteger('MTB'+IntToStr(i),'scom',0);
    end;
 finally
   Ini.Free();
 end;
end;//function

procedure TFormConfig.SaveData(filename:string);
var Ini:TMemIniFile;
    i:Integer;
begin
 Ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   ini.WriteInteger('MTB', 'start', Self.pins_start);
   ini.WriteInteger('MTB', 'end', Self.pins_end);

   for i := 0 to _MAX_MTB do
    begin
     if (Modules[i].name <> '') then
       Ini.WriteString('MTB'+IntToStr(i),'name',Modules[i].name);
     if (Modules[i].typ <> idMTB_UNI_ID) then
       Ini.WriteInteger('MTB'+IntToStr(i),'typ', Integer(Modules[i].typ));
     if (Modules[i].fw <> 'VIRTUAL') then
       Ini.WriteString('MTB'+IntToStr(i),'fw',Modules[i].fw);
     if (Modules[i].exists) then
       Ini.WriteBool('MTB'+IntToStr(i),'is',Modules[i].exists);
     if (Modules[i].ir <> 0) then
       Ini.WriteInteger('MTB'+IntToStr(i),'ir',Modules[i].ir);
     if (Modules[i].scom <> 0) then
       Ini.WriteInteger('MTB'+IntToStr(i),'scom',Modules[i].scom);
    end;
 finally
   Ini.UpdateFile();
   Ini.Free();
 end;
end;

procedure TFormConfig.SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then
  Self.B_ErrorClick(Self);
end;//procedure

procedure TFormConfig.B_ErrorClick(Sender: TObject);
begin
 if (Assigned(LibEvents.OnError.event)) then
  LibEvents.OnError.event(Self, LibEvents.OnError.data, Self.SE_Err_id.Value, Self.SE_Err_board.Value, '');
end;

procedure TFormConfig.CfgBtnOnClick(Sender:TObject);
begin
 F_Board.OpenForm((Sender as TButton).Tag);
end;//procedure

procedure TFormConfig.CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 Caption := Format('%.3d', [(Sender as TButton).Tag]);
end;//procedure

procedure TFormConfig.SetStatus(new:TSimulatorStatus);
begin
 case (new) of
  TSimulatorStatus.closed   : Self.Caption := 'Closed';
  TSimulatorStatus.opening  : Self.Caption := 'Opening...';
  TSimulatorStatus.stopped  : Self.Caption := 'Openned, stopped';
  TSimulatorStatus.starting : Self.Caption := 'Starting...';
  TSimulatorStatus.running  : Self.Caption := 'Running';
  TSimulatorStatus.stopping : Self.Caption := 'Stopping...';
  TSimulatorStatus.closing  : Self.Caption := 'Closing...';
 end;//case

 Self.fstatus := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// events from simulator timer:

procedure TFormConfig.OnOpen(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.stopped;
  if (Assigned(LibEvents.AfterOpen.event)) then LibEvents.AfterOpen.event(FormConfig, LibEvents.AfterOpen.data);
end;//procedure

procedure TFormConfig.OnClose(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.closed;
  if (Assigned(LibEvents.AfterClose.event)) then LibEvents.AfterClose.event(FormConfig, LibEvents.AfterClose.data);
end;//procedure

procedure TFormConfig.OnStart(Sender:TObject);
begin
  status := TSimulatorStatus.running;

  if (F_Board.Showing) then
    F_Board.RG_Failure.Enabled := Modules[F_Board.OpenIndex].exists;

  if (Assigned(LibEvents.AfterStart.event)) then LibEvents.AfterStart.event(FormConfig, LibEvents.AfterStart.data);

  (Sender as TTimer).Interval := 500;
  (Sender as TTimer).OnTimer := Self.OnScanned;
end;//procedure

procedure TFormConfig.OnStop(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.stopped;
  F_Board.RG_Failure.Enabled := false;
  F_Board.RG_Exists.Enabled := true;
  if (Assigned(LibEvents.AfterStop.event)) then LibEvents.AfterStop.event(FormConfig, LibEvents.AfterStop.data);
end;//procedure

procedure TFormConfig.OnScanned(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  if (Assigned(LibEvents.OnScanned.event)) then LibEvents.OnScanned.event(FormConfig, LibEvents.OnScanned.data);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
  FreeAndNil(FormConfig);

end.
