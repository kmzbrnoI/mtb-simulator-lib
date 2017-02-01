////////////////////////////////////////////////////////////////////////////////
// Errors.pas
//  MTB communication library
//  Error codes definiton
//   (c) Jan Horacek (jan.horacek@kmz-brno.cz),
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2016 Jan Horacek

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

  This file defines library error codes.
}

unit Errors;

interface

const
 MTB_GENERAL_EXCEPTION = 1000;
 MTB_FT_EXCEPTION = 1001;       // device is always closed when this exception happens
 MTB_FILE_CANNOT_ACCESS = 1010;
 MTB_FILE_DEVICE_OPENED = 1011;
 MTB_MODULE_INVALID_ADDR = 1100;
 MTB_MODULE_FAILED = 1102;
 MTB_PORT_INVALID_NUMBER = 1103;
 MTB_MODULE_UNKNOWN_TYPE = 1104;
 MTB_INVALID_SPEED = 1105;
 MTB_INVALID_SCOM_CODE = 1106;
 MTB_INVALID_MODULES_COUNT = 1107;
 MTB_INPUT_NOT_YET_SCANNED = 1108;

 MTB_ALREADY_OPENNED = 2001;
 MTB_CANNOT_OPEN_PORT = 2002;
 MTB_FIRMWARE_TOO_LOW = 2003;
 MTB_DEVICE_DISCONNECTED = 2004;
 MTB_SCANNING_NOT_FINISHED = 2010;
 MTB_NOT_OPENED = 2011;
 MTB_ALREADY_STARTED = 2012;
 MTB_OPENING_NOT_FINISHED = 2021;
 MTB_NO_MODULES = 2025;
 MTB_NOT_STARTED = 2031;

 MTB_INVALID_PACKET = 3100;
 MTB_MODULE_NOT_ANSWERED_CMD = 3101;
 MTB_MODULE_NOT_ANSWERED_CMD_GIVING_UP = 3102;
 MTB_MODULE_OUT_SUM_ERROR = 3106;
 MTB_MODULE_OUT_SUM_ERROR_GIVING_UP = 3107;
 MTB_MODULE_IN_SUM_ERROR = 3108;
 MTB_MODULE_IN_SUM_ERROR_GIVING_UP = 3109;

 MTB_MODULE_NOT_RESPONDED_FB = 3121;
 MTB_MODULE_NOT_RESPONDED_FB_GIVING_UP = 3122;
 MTB_MODULE_IN_FB_SUM_ERROR = 3126;
 MTB_MODULE_IN_FB_SUM_ERROR_GIVING_UP = 3127;
 MTB_MODULE_OUT_FB_SUM_ERROR = 3128;
 MTB_MODULE_OUT_FB_SUM_ERROR_GIVING_UP = 3129;
 MTB_MODULE_INVALID_FB_SUM = 3125;
 MTB_MODULE_NOT_RESPONDING_PWR_ON = 3131;

 MTB_MODULE_PWR_ON_IN_SUM_ERROR = 3136;
 MTB_MODULE_PWR_ON_IN_SUM_ERROR_GIVING_UP = 3137;
 MTB_MODULE_PWR_ON_OUT_SUM_ERROR = 3138;
 MTB_MODULE_PWR_ON_OUT_SUM_ERROR_GIVING_UP = 3139;

 MTB_MODULE_FAIL = 3141;
 MTB_MODULE_RESTORED = 3142;
 MTB_MODULE_INVALID_DATA = 3145;

 MTB_MODULE_REWIND_IN_SUM_ERROR = 3162;
 MTB_MODULE_REWIND_OUT_SUM_ERROR = 3163;

 MTB_MODULE_SCAN_IN_SUM_ERROR = 3166;
 MTB_MODULE_SCAN_IN_SUM_ERROR_GIVING_UP = 3167;
 MTB_MODULE_SCAN_OUT_SUM_ERROR = 3168;
 MTB_MODULE_SCAN_OUT_SUM_ERROR_GIVING_UP = 3169;

 MTB_MODULE_SC_IN_SUM_ERROR = 3176;
 MTB_MODULE_SC_IN_SUM_ERROR_GIVING_UP = 3177;
 MTB_MODULE_SC_OUT_SUM_ERROR = 3178;
 MTB_MODULE_SC_OUT_SUM_ERROR_GIVING_UP = 3179;

implementation

end.
