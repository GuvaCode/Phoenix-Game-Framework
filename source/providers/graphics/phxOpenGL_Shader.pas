////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxOpenGL_Shader;
//< Helper functions for creating OpenGL shaders

interface

{$I ../../phxConfig.inc}

uses
  SysUtils, Classes,

  dglOpenGL,

  phxLogger,
  phxDevice;

function GetInfoLog(glObject : GLHandleARB) : String;

function CompileShader(const Kind: GLuint; const Source: AnsiString): GLuint;
function CompileProgram(const Vert, Frag: GLuint): GLuint;

implementation


//------------------------------------------------------------------------------
function GetInfoLog(glObject : GLHandleARB) : String;
var blen: GLInt;
var slen: GLInt;
var text: AnsiString;
begin
  blen:= 0;
  slen:= 0;

  glGetObjectParameterivARB(glObject, GL_OBJECT_INFO_LOG_LENGTH_ARB , @blen);

  text:= '';

  if blen > 1 then
  begin
    SetLength(text, blen);

    glGetInfoLogARB(glObject, blen, slen, @text[1]);
  end;

  Result:= String(text);
end;

//------------------------------------------------------------------------------
function CompileShader(const Kind: GLuint; const Source: AnsiString): GLuint;
var Error         : String;
var Compiled        : GLint;
begin
  Result:= glCreateShaderObjectARB( Kind   ) ;

  glShaderSourceARB(Result, 1, @Source, nil);

  // Compile the shader
  glCompileShaderARB(Result);

  // Get the compilation status of the shader
  glGetObjectParameterivARB(Result, GL_OBJECT_COMPILE_STATUS_ARB, @Compiled);

  if Compiled = GL_FALSE then
  begin
    Error:= GetInfoLog(Result);

    raise Exception.CreateFmt('Shader compilation error:'#13'%s', [Error]);
  end;
end;

//------------------------------------------------------------------------------
function CompileProgram(const Vert, Frag: GLuint): GLuint;
var Error : String;
var Linked: GLint;
begin
  Result:= glCreateProgram();

  glAttachShader(Result, Vert);
  glAttachShader(Result, Frag);

  // Link the program
  glLinkProgram(Result);

  glGetObjectParameterivARB(Result, GL_OBJECT_LINK_STATUS_ARB, @Linked);

  if Linked = GL_FALSE then
  begin
    Error:= GetInfoLog(Result);

    raise Exception.CreateFmt('Failed to link the shader program'#13'%s', [Error]);
  end;
end;


end.
