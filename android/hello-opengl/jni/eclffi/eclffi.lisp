(defpackage :eclffi
  (:use :cl :ffi :util)
  (:export
   :gl-init
   :gl-prepare-frame
   :draw-test-line))

(in-package :eclffi)

(clines 
"#include <ecl/ecl.h>
#include <GLES/gl.h>")

(defun gl-init ()
  "initialize OpenGL state"
  (ffi:c-inline () () :void
  "{
    glEnable(GL_NORMALIZE);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glShadeModel(GL_FLAT);

    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    glEnable(GL_LIGHT2);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
  }"))

(defun gl-prepare-frame (width height)
  "prepare the state of OpenGL for drawing a frame"
  (c-inline (width height) (:int :int) :void
  "{
    glViewport(0, 0, #0, #1);

    glClearColorx((GLfixed)(0.5f * 65536),
                  (GLfixed)(0.5f * 65536),
                  (GLfixed)(0.5f * 65536), 
                  (GLfixed)(1.0f * 65536));
    glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    //gluPerspective(45, (float)width / height, 0.5f, 150);
    {
    GLfloat fovy = 45;
    GLfloat aspect = (float)#0 / #1;
    GLfloat zNear = 0.5f;
    GLfloat zFar = 150;

    GLfloat xmin, xmax, ymin, ymax;

    ymax = zNear * (GLfloat)tan(fovy * 3.1416 / 360);
    ymin = -ymax;
    xmin = ymin * aspect;
    xmax = ymax * aspect;

    glFrustumx((GLfixed)(xmin * 65536), (GLfixed)(xmax * 65536),
               (GLfixed)(ymin * 65536), (GLfixed)(ymax * 65536),
               (GLfixed)(zNear * 65536), (GLfixed)(zFar * 65536));
    }

    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();
  }"))

(clines "GLfloat float_buffer[16];" )

(defun draw-test-line (x0 y0 x1 y1 r g b a)
  "test function, just to see how to draw on the screen from Lisp
   using OpenGL"
  (c-inline (x0 y0 x1 y1 r g b a) 
		(:float :float :float :float :float :float :float :float) 
		:void 
 "{
   float_buffer[0] = #0;
   float_buffer[1] = #1;
   float_buffer[2] = #2;
   float_buffer[3] = #3;

   glVertexPointer(2, GL_FLOAT, 0, float_buffer);
   glColor4f(#4, #5, #6, #7);
   glDrawArrays(GL_LINES, 0, 2);
  }"))


