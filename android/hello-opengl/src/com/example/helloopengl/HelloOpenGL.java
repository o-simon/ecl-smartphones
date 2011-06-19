/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.example.helloopengl;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.app.Activity;
import android.content.Context;
import android.opengl.GLSurfaceView;
import android.os.Bundle;
import android.view.MotionEvent;

import android.content.res.AssetManager;
import android.content.res.AssetFileDescriptor;
import android.widget.TextView;

import android.util.Log;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.RandomAccessFile;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileOutputStream;
import java.io.OutputStream;

public class HelloOpenGL extends Activity {
    private static String TAG = "HelloOpenGL";
    private static String RESOURCES_DIR = "lisp";
    private static String APP_RESOURCES_DIR = "resources";
    private static boolean DEBUG = false;
    
    static AssetManager assetManager;
    static File uncompressedFilesDir;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mGLView = new HelloOpenGLSurfaceView(this);
        setContentView(mGLView);

        assetManager = getAssets();

        uncompressedFilesDir = getDir(APP_RESOURCES_DIR,MODE_PRIVATE);
        uncompressDir(RESOURCES_DIR,uncompressedFilesDir);

        /* Create a TextView and set its content.
         * the text is retrieved by calling a native
         * function.
         */
        // TextView  tv = new TextView(this);
        // tv.setText("DONE");
        // setContentView(tv);
    }

    @Override
    protected void onPause() {
        super.onPause();
        mGLView.onPause();
    }

    @Override
    protected void onResume() {
        super.onResume();
        mGLView.onResume();
    }

    private GLSurfaceView mGLView;

    public void uncompressDir(String in, File out) {
        try {
            String[] files = assetManager.list(in);
            Log.w(TAG,"Uncompressing: " + files.length + " files");
            for(int i=0; i<files.length; i++) {
                Log.w(TAG,"Uncompressing: " + files[i]);
                File fileIn = new File(in,files[i]);
                File fileOut = new File(out,files[i]);

                try {
                    uncompressFile(fileIn,fileOut);
                } catch(FileNotFoundException e) {
                    // fileIn is a directory, uncompress the subdir
                    if(!fileOut.isDirectory()) {
                        Log.w(TAG,"Creating dir: " + fileOut.getAbsolutePath());
                        fileOut.mkdir();
                    }
                    uncompressDir(fileIn.getPath(), fileOut);    
                }
            }
	} catch(IOException e) {
	    e.printStackTrace();
	}
    }

    public static String getResourcesPath() {
	return uncompressedFilesDir.getAbsolutePath();
    }

    public static void uncompressFile(File fileIn,File fileOut) throws IOException {
        InputStream in = assetManager.open(fileIn.getPath(),
                                           android.content.res.AssetManager.ACCESS_RANDOM);
	OutputStream out = new FileOutputStream(fileOut);
	
	byte[] buf = new byte[1024];
	int len;
	while ((len = in.read(buf)) > 0) {
	    out.write(buf, 0, len);
	}
	
	in.close();
	out.close();
	Log.i(TAG,"File copied.");
    }
    
    static {
        System.loadLibrary("hello-opengl");
        Log.w(TAG,"Done loading hello-opengl library");
    }
}

class HelloOpenGLSurfaceView extends GLSurfaceView {
    private static String TAG = "GLSurfaceView";

    public HelloOpenGLSurfaceView(Context context) {
        super(context);
        mRenderer = new HelloOpenGLRenderer();
        setRenderer(mRenderer);
    }

    public boolean onTouchEvent(final MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            nativeRegisterTouch( event.getX(), event.getY() );
        }
        return true;
    }

    HelloOpenGLRenderer mRenderer;

    private static native void nativeRegisterTouch(float x, float y);
}

class HelloOpenGLRenderer implements GLSurfaceView.Renderer {
    private static String TAG = "HelloOpenGLRenderer";

    public void onSurfaceCreated(GL10 gl, EGLConfig config) {
        Log.i(TAG,"ECL Starting...");        
        startECL();
        Log.i(TAG,"ECL Started");

        Log.i(TAG,"going to nativeInit");        
        nativeInit();
        Log.i(TAG,"finished nativeInit");        
    }

    public void onSurfaceChanged(GL10 gl, int w, int h) {
        //gl.glViewport(0, 0, w, h);
        nativeResize(w, h);
    }

    public void onDrawFrame(GL10 gl) {
        nativeStep();
    }

    public static String getResourcesPath(){
	return HelloOpenGL.getResourcesPath();
    }

    public native void startECL();
    
    private static native void nativeInit();
    private static native void nativeResize(int w, int h);
    private static native void nativeStep();
    //private static native void nativeDone();
}

