unit main ;

interface
uses sevenzip, SysUtils ;

type
  TMain = class
  private
    compressionlevel:Integer ;
    compressionmethod:TZipCompressionMethod ;
    zipfile:string ;
    function SetMethodByStr(str:string):Boolean ;
    function ParseCommand(const s:string; var cmd:string;
      var args:TArray<string>):Boolean ;
    procedure ExitWithError(const msg:string; code:Integer = 1) ;
  public
    procedure Run() ;
  end;

implementation
uses Classes ;

procedure TMain.ExitWithError(const msg: string; code: Integer);
begin
  Writeln(msg) ;
  Halt(code) ;
end;

function TMain.ParseCommand(const s: string; var cmd: string;
  var args: TArray<string>): Boolean;
var arr:TArray<string> ;
    i:Integer ;
begin
  // В текущей реализации не поддерживается парсинг с кавычками - пробелы в именах нельзя
  // Нужно либо сделать чистовой паросер, либо убирать кавычки при возрате строки Split
  arr:=s.Split([' '],'"') ;
  if Length(arr)<2 then ExitWithError('Not found arguments: '+s) ;
  cmd:=arr[0] ;
  SetLength(args,Length(arr)-1) ;
  for i := 0 to Length(arr)-2 do
   args[i]:=arr[i+1] ;
  Result:=True ;
end;

procedure TMain.Run() ;
var arc: I7zOutArchive;
    script:TStringList ;
    stms:TStringStream ;
    s,cmd,resfile:string ;
    args:TArray<string> ;
begin
  try
    Writeln('This source is prototype for development purposes only!!!') ;
    if ParamCount<1 then ExitWithError('Usage: scriptfile') ;

    compressionlevel:=5 ;
    compressionmethod:=TZipCompressionMethod.mzDeflate ;
    zipfile:='' ;

    arc := CreateOutArchive(CLSID_CFormatZip);

    if not FileExists(ParamStr(1)) then
      ExitWithError('Not found script file: '+ParamStr(1)) ;

    script:=TStringList.Create() ;
    script.LoadFromFile(ParamStr(1)) ;
    for s in script do begin
      if s.Trim().Length=0 then Continue ;
      if s.Trim().StartsWith('#') then Continue ;
      if not ParseCommand(s,cmd,args) then
        ExitWithError('Error parsing line: '+s) ;
      cmd:=cmd.ToLower() ;
      if cmd='setcompressor' then begin
        if not SetMethodByStr(args[0]) then
          ExitWithError('Unknown compression method: '+args[0]) ;
      end;
      if cmd='zipfile' then begin
        zipfile:=args[0] ;
        Writeln('Setting zipfile: '+zipfile) ;
      end;
      if cmd='file' then begin
        if not FileExists(args[0]) then ExitWithError('Not found file: '+args[0]) ;
        if Length(args)>1 then resfile:=args[1] else resfile:=ExtractFileName(args[0]) ;
        arc.AddFile(args[0], resfile) ;
        Writeln('Adding file: '+args[0]+' as '+resfile) ;
      end;
      if cmd='text' then begin
        if Length(args)<2 then ExitWithError('Command "Text" required two arguments: text and filename in archive') ;
        stms:=TStringStream.Create(args[0]) ;
        arc.AddStream(stms, soOwned, faArchive, CurrentFileTime, CurrentFileTime, args[1], false, false);
        Writeln('Adding string: '+args[0]+' as '+args[1]) ;
      end;
    end;

    script.Free ;

    if zipfile='' then ExitWithError('Not set ZipFile') ;

    SetCompressionLevel(arc, 5);
    SetCompressionMethod(arc, compressionmethod) ;

    if FileExists(zipfile) then DeleteFile(zipfile) ;
    
    arc.SaveToFile(zipfile);
    Writeln('Archive build OK') ;
  except
    on E: Exception do
      Writeln('Error '+E.ClassName+': '+E.Message);
  end;
end ;

function TMain.SetMethodByStr(str: string):Boolean;
begin
  Result:=True ;
  str:=str.ToLower() ;
  if str='deflate' then compressionmethod:=TZipCompressionMethod.mzDeflate else
  if str='deflate64' then compressionmethod:=TZipCompressionMethod.mzDeflate64 else
  if str='bzip2' then compressionmethod:=TZipCompressionMethod.mzBZip2 else
  if str='lzma' then compressionmethod:=TZipCompressionMethod.mzLZMA else
  if str='ppmd' then compressionmethod:=TZipCompressionMethod.mzPPMD else
  Result:=False ;
end;

end.
