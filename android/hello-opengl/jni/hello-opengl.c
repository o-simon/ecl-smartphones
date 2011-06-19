#include <assert.h>
#include <android/log.h>
#include <string.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>

#include <ecl/ecl.h>
#include "ecl_boot.h"

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, "native-activity", __VA_ARGS__))


void Java_com_example_helloopengl_HelloOpenGLRenderer_startECL( JNIEnv* env, jobject thiz ){
  LOGI("INIT ECL");
  jclass cls = (*env)->GetObjectClass(env, thiz);
  assert(cls);
  jmethodID mid = (*env)->GetStaticMethodID(env, cls, "getResourcesPath", "()Ljava/lang/String;");
  assert(mid);
  
  jstring file = (*env)->CallStaticObjectMethod(env, cls, mid);
  const char *lisp_dir = (*env)->GetStringUTFChars(env, file, NULL);
  
  LOGI("Path is: %s\n",lisp_dir);
  
  ecl_boot(lisp_dir);
  LOGI("INIT ECL DONE");	
}

void Java_com_example_helloopengl_HelloOpenGLRenderer_nativeInit( JNIEnv* env, jobject thiz ){
  LOGI("going to init");	
  Lisp_init();
  LOGI("finished init");	
}

void Java_com_example_helloopengl_HelloOpenGLRenderer_nativeStep( JNIEnv* env, jobject thiz ){
  Lisp_main_loop_iteration();
}

void Java_com_example_helloopengl_HelloOpenGLRenderer_nativeResize( JNIEnv* env, jobject thiz, jint w, jint h ){
  LOGI("going to set_window_size w=%d, h=%d", w, h);	
  Lisp_set_window_size(w, h);
  LOGI("finished set_window_size");	
}

void Java_com_example_helloopengl_HelloOpenGLSurfaceView_nativeRegisterTouch( JNIEnv* env, jobject thiz, jfloat x, jfloat y ){
  LOGI("going to register_touch x=%f, y=%f", x, y);	
  Lisp_register_touch(x, y);
  LOGI("finished register_touch");	
}
