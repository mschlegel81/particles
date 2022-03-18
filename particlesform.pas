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
  viewWrapper;

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
    FormResize(self);
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
  VAR upperLeftOnScreen: TPoint;
      heightDelta:longint;
  begin
    if not(biSystemMenu in BorderIcons) then begin
      SetBounds((screen.width-800) div 2,(screen.height-600) div 2,800,600);
      BorderIcons:=[biSystemMenu,biMaximize];
      FormStyle:=fsNormal;
      //BorderStyle:=bsSizeable;
      WindowState:=wsNormal;
    end else begin
      BorderIcons:=[];
      FormStyle:=fsStayOnTop;
      //!Switching BorderStyle breaks OpenGlControl!
      //BorderStyle:=bsNone;
      WindowState:=wsMaximized;
      upperLeftOnScreen:=ClientToScreen(point(0,0));
      heightDelta:=upperLeftOnScreen.Y-top;
      top :=top-heightDelta;
      height:=height+heightDelta;
    end;
  end;

// ---------------------------------------------------------------------------
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ---------------------------------------------------------------------------

PROCEDURE TExampleForm.IdleFunc(Sender: TObject; VAR done: boolean);
  begin
    if not(Assigned(OpenGLControl1)) then exit;
    OpenGLControl1.Invalidate;
    done:=false; // tell lcl to handle messages and return immediatly
  end;

PROCEDURE TExampleForm.FormResize(Sender: TObject);
  begin
    if OpenGLControl1<>nil then
      OpenGLControl1.SetBounds(0, 0, width, height);
  end;

end.
