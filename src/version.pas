﻿unit version;

interface

uses Windows, SysUtils;

 function VersionStr(const FileName: string): string; //cteni verze z nastaveni

implementation

function VersionStr(const FileName: string): string;//cteni verze z nastaveni
var
  size, len: longword;
  handle: Cardinal;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor, Release: word;
begin
  Result := 'Není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if (size > 0) then
   begin
    GetMem(buffer, size);
    if ((GetFileVersionInfo(Pointer(FileName), 0, size, buffer)) and
        (VerQueryValue(buffer, '\', pointer(pinfo), len))) then
     begin
      Major := HiWord(pinfo.dwFileVersionMS);
      Minor := LoWord(pinfo.dwFileVersionMS);
      Release := HiWord(pinfo.dwFileVersionLS);
      Result := Format('%d.%d.%d',[Major, Minor, Release]);
     end;
    FreeMem(buffer);
  end;
end;

end.//unit
