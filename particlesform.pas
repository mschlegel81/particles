{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General public License as published by  *
 *   the free Software Foundation; either VERSION 2 of the License, or     *
 *   (at your option) any later VERSION.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT any WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the GNU     *
 *   General public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General public License is available on the world    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the free Software Foundation,                 *
 *   inc., 51 Franklin Street - Fifth floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

written 2022 by Martin Schlegel

}
UNIT particlesForm;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, sysutils, LCLProc, Forms,
  OpenGLContext,Controls,
  viewWrapper,
  settings;

TYPE
  TExampleForm = class(TForm)
    OpenGLControl1: TOpenGLControl;

    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
    PROCEDURE OpenGlControl1DblClick(Sender: TObject);
  public
    viewState: T_viewState;
    CONSTRUCTOR create(TheOwner: TComponent); override;
    DESTRUCTOR destroy; override;
  private

    PROCEDURE initOpenGlControl;
  end;

VAR AnExampleForm: TExampleForm;
IMPLEMENTATION

CONSTRUCTOR TExampleForm.create(TheOwner: TComponent);
  begin
    inherited CreateNew(TheOwner);
    randomize;

    SetBounds((screen.width-800) div 2,(screen.height-600) div 2,800,600);
    caption:='Particles...';
    Application.OnIdle:=@IdleFunc;
    OnResize          :=@FormResize;
    FormResize(self);

    initOpenGlControl;
    viewState:=T_viewState.create(OpenGLControl1);
    sharedViewState:=viewState;
    FormResize(self);

    if (paramCount>=1) and (paramStr(1)='-windowed') then begin
      currentlyWindowed:=true;
    end else begin
      currentlyWindowed:=false;
      WindowState:=wsFullScreen;
      BorderStyle:=bsNone;
    end;
  end;

DESTRUCTOR TExampleForm.destroy;
  begin
    FreeAndNil(viewState);
    inherited destroy;
  end;

PROCEDURE TExampleForm.initOpenGlControl;
  begin
    OpenGLControl1:=TOpenGLControl.create(self);
    with OpenGLControl1 do begin
      name:='OpenGLControl1';
      parent:=self;
      OnDblClick :=@OpenGlControl1DblClick;
    end;
    OpenGLControl1.SetBounds(0, 0, width, height);
  end;

PROCEDURE TExampleForm.OpenGlControl1DblClick(Sender: TObject);
  begin
    if isSettingsFormShowing then getSettingsForm.Hide
    else begin
      getSettingsForm.top:=top;
      getSettingsForm.Left:=Left;
      getSettingsForm.Show;
    end;
  end;

PROCEDURE TExampleForm.IdleFunc(Sender: TObject; VAR done: boolean);
  begin
    if not(Assigned(OpenGLControl1)) then exit;
    if isSettingsFormShowing then begin
      if currentlyWindowed and ((getSettingsForm.top<>top) or (getSettingsForm.Left<>Left)) then begin
        getSettingsForm.top:=top;
        getSettingsForm.Left:=Left;
      end;
      getSettingsForm.BringToFront;
    end;
    OpenGLControl1.Invalidate;
    done:=false; // tell lcl to handle messages and return immediatly
  end;

PROCEDURE TExampleForm.FormResize(Sender: TObject);
  begin
    if OpenGLControl1<>nil then
      OpenGLControl1.SetBounds(0, 0, width, height);
    if isSettingsFormShowing then begin
      getSettingsForm.top:=top;
      getSettingsForm.Left:=Left;
    end;
  end;

end.
