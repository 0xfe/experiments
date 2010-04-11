#include "asterisk.h"

ASTERISK_FILE_VERSION(__FILE__, "blah");

#include "asterisk/module.h"
#include "asterisk/logger.h"

static int load_module(void) {
  ast_log(LOG_NOTICE, "Hello World!\n");
  return AST_MODULE_LOAD_SUCCESS;
}

static int unload_module(void) {
  ast_log(LOG_NOTICE, "Goodbye World!\n");
  return 0;
}

AST_MODULE_INFO_STANDARD(ASTERISK_GPL_KEY, "Hello World");
