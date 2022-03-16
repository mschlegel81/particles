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
  Classes, sysutils, LazFileUtils, LazUTF8, LCLProc, Forms, LResources,
  Dialogs, Graphics, GL, FPimage, OpenGLContext,Controls,particlePhysics,vectors;

TYPE
  TExampleForm = class(TForm)
    OpenGLControl1: TOpenGLControl;

    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE OpenGLControl1Paint(Sender: TObject);
    PROCEDURE OpenGLControl1Resize(Sender: TObject);
    PROCEDURE OpenGlControl1DblClick(Sender: TObject);
  public
    CONSTRUCTOR create(TheOwner: TComponent); override;
    DESTRUCTOR destroy; override;
  private
    AreaInitialized: boolean;
    frameCount    : integer;
    LastFrameTicks: integer;
    modeTicks     : integer;

    mouseX,mouseY : longint;
    mouseIsDown   : byte;
    PROCEDURE initOpenGlControl;
    PROCEDURE OpenGLControl1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE OpenGLControl1MouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  end;

VAR AnExampleForm: TExampleForm;
CONST TARGET_FPS=50;

IMPLEMENTATION

VAR rx: single=0;
    ry: single=0;
    ParticleEngine: TParticleEngine;
    ParticleList: GLuint;
    sleepTimeMilliseconds:longint=0;

CONSTRUCTOR TExampleForm.create(TheOwner: TComponent);
  begin
    inherited CreateNew(TheOwner);
    randomize;
    if LazarusResources.find(ClassName)=nil then begin
      SetBounds((screen.width-800) div 2,(screen.height-600) div 2,800,600);
      caption:='Particles...';
      Application.OnIdle:=@IdleFunc;
      OnResize          :=@FormResize;
      FormResize(self);
      ParticleEngine:=TParticleEngine.create;
      initOpenGlControl;
    end;
    FormResize(self);
  end;

DESTRUCTOR TExampleForm.destroy;
  begin
    FreeAndNil(ParticleEngine);
    inherited destroy;
  end;

PROCEDURE TExampleForm.initOpenGlControl;
  begin
    OpenGLControl1:=TOpenGLControl.create(self);
    with OpenGLControl1 do begin
      name:='OpenGLControl1';
      parent:=self;
      OnPaint    :=@OpenGLControl1Paint;
      OnResize   :=@OpenGLControl1Resize;
      OnDblClick :=@OpenGlControl1DblClick;
      OnMouseDown:=@OpenGLControl1MouseDown;
      OnMouseMove:=@OpenGLControl1MouseMove;
      OnMouseUp  :=@OpenGLControl1MouseUp;
      mouseX :=0;
      mouseY :=0;
      mouseIsDown:=0;
    end;
    OpenGLControl1.SetBounds(0, 0, width, height);
  end;

PROCEDURE TExampleForm.OpenGLControl1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    mouseX:=x;
    mouseY:=y;
    case button of
      mbLeft : mouseIsDown:=1;
      mbRight: mouseIsDown:=2;
    end;
  end;

PROCEDURE TExampleForm.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if mouseIsDown<>1 then exit;
    ry+=(x-mouseX)/width*180;
    rx+=(y-mouseY)/height*180;
    mouseX:=x;
    mouseY:=y;
  end;

PROCEDURE TExampleForm.OpenGLControl1MouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if mouseIsDown=2 then ParticleEngine.nextSetup(modeTicks);
    mouseIsDown:=0;
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
    sleep(sleepTimeMilliseconds);
    done:=false; // tell lcl to handle messages and return immediatly
  end;

PROCEDURE TExampleForm.FormResize(Sender: TObject);
  begin
    if OpenGLControl1<>nil then
      OpenGLControl1.SetBounds(0, 0, width, height);
  end;

VAR lightamb, lightdif, lightpos: array [0..3] of GLfloat;
PROCEDURE TExampleForm.OpenGLControl1Paint(Sender: TObject);
  CONST GLInitialized: boolean = false;

  PROCEDURE InitGL;
    CONST C_icosahedronFaces:array[0..19,0..2] of byte=
      ((1,2,0),(0, 4,3),( 5, 6,1),(3, 7, 5),
       (2,9,8),(8,10,4),(11, 9,6),(7,10,11),
       (0,3,1),(1, 3,5),( 9,10,8),(11,10,9),
       (2,8,0),(0, 8,4),( 5,11,6),( 7,11,5),
       (1,6,2),(4, 7,3),( 2, 6,9),(10, 7,4));
    VAR n:TVector3;
        i,j:longint;
    begin
      if GLInitialized then exit;
      GLInitialized:=true;

      {setting lighting conditions}
      {ambient color}
      lightamb[0]:=0.2;
      lightamb[1]:=0.2;
      lightamb[2]:=0.2;
      lightamb[3]:=1.0;
      {diffuse color}
      lightdif[0]:=0.5;
      lightdif[1]:=0.5;
      lightdif[2]:=0.5;
      lightdif[3]:=1.0;
      {diffuse position}
      lightpos[0]:=0.0;
      lightpos[1]:=1.0;
      lightpos[2]:=0.0;
      lightpos[3]:=0.0;

      glLightfv(GL_LIGHT0,GL_AMBIENT ,lightamb);
      glLightfv(GL_LIGHT1,GL_DIFFUSE ,lightdif);
      glLightfv(GL_LIGHT1,GL_POSITION,lightpos);

      glEnable(GL_LIGHT0);
      glEnable(GL_LIGHT1);

      glClearColor(0.0,0.0,0.0,0.1);    // sets background color
      glClearDepth(1.0);
      glDepthFunc(GL_LEQUAL);           // the type of depth test to do
      glEnable(GL_DEPTH_TEST);          // enables depth testing
      glShadeModel(GL_SMOOTH);          // enables smooth color shading
      glColor4f(0.7,0.7,0.7,1.0);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
      glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
      glEnable( GL_BLEND );
      glEnable(GL_COLOR_MATERIAL);
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);

      ParticleList:=glGenLists(1);

      glColor3f(1,0.5,0);
      glNewList(ParticleList, GL_COMPILE);
        glBegin(GL_TRIANGLES);
          for i:=0 to 19 do begin
            for j:=0 to 2 do begin
              n:=C_IcosahedronNodes[C_icosahedronFaces[i,j]];
              n*=1/euklideanNorm(n);
              glNormal3f(n[0],n[1],n[2]);
              n*=0.013;
              glVertex3f(n[0],n[1],n[2]);
            end;
          end;
        glEnd;
      glEndList;
      glEnable(GL_LIGHTING);
    end;

  CONST az=1;
  VAR ax:double=az;
      ay:double=az;
      timer:single;
  begin
    inc(frameCount);
    inc(modeTicks     ,OpenGLControl1.FrameDiffTimeInMSecs);
    inc(LastFrameTicks,OpenGLControl1.FrameDiffTimeInMSecs);
    if (LastFrameTicks>=1000) then begin
      dec(LastFrameTicks,1000);
      if      frameCount>TARGET_FPS*1.5 then inc(sleepTimeMilliseconds)
      else if frameCount<TARGET_FPS     then dec(sleepTimeMilliseconds);
      if sleepTimeMilliseconds<0 then sleepTimeMilliseconds:=0;
      DebugLn(['TExampleForm.OpenGLControl1Paint Frames per second: ',frameCount,' sleeping for: ',sleepTimeMilliseconds,'ms']);
      caption:='Particles ('+intToStr(frameCount)+'fps, '+intToStr(sleepTimeMilliseconds)+'ms sleep)';
      frameCount:=0;
    end;

    if OpenGLControl1.MakeCurrent then
    begin
      if not AreaInitialized then begin
        InitGL;
        glMatrixMode (GL_PROJECTION);    { prepare for and then }
        glLoadIdentity ();               { define the projection }

        glFrustum (-0.1, 0.1, -0.1, 0.1, 0.35, 20.0); { transformation }
        glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
        glViewport (0, 0, OpenGLControl1.width, OpenGLControl1.height);
                                      { define the viewport }
        AreaInitialized:=true;
      end;

      timer:=ParticleEngine.update(modeTicks);

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;             { clear the matrix }
      glPushMatrix;

      if (mouseIsDown<>1) then begin
        if ParticleEngine.currentAttractionMode=CLOCK_TARGET
        then ry-=ry*timer
        else ry+=10*timer;
        rx-=rx*timer;

        if ry> 180 then ry-=360;
        if ry<-180 then ry+=360;
        if rx> 180 then rx-=360;
        if rx<-180 then rx+=360;
      end;

      if OpenGLControl1.width>OpenGLControl1.height
      then ax*=OpenGLControl1.height/OpenGLControl1.width
      else ay*=OpenGLControl1.width/OpenGLControl1.height;

      glTranslatef(0,0,-5);

      glScalef(ax,ay,az);
      glRotatef(rx,0.1,0.0,0.0);
      glRotatef(ry,0.0,1.0,0.0);

      glEnable(GL_BLEND);
      ParticleEngine.DrawParticles(ParticleList);
      glDisable(GL_BLEND);

      glPopMatrix;
      OpenGLControl1.SwapBuffers;
    end;
  end;

PROCEDURE TExampleForm.OpenGLControl1Resize(Sender: TObject);
  begin
    if (AreaInitialized) and OpenGLControl1.MakeCurrent then begin
      glViewport (0, 0, OpenGLControl1.width, OpenGLControl1.height);
    end;
  end;

end.
