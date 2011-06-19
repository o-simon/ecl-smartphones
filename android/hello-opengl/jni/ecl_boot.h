#ifndef _ECL_BOOT_H_
#define _ECL_BOOT_H_

int ecl_boot(const char *root_dir);
void ecl_toplevel(const char *home);
void eclshell_show(char *message);

void Lisp_init();
void Lisp_main_loop_iteration();
void Lisp_set_window_size(int width, int height);
void Lisp_register_touch(float width, float height);

#endif
