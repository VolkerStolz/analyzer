#include <pthread.h>

int data[10];

void *t_fun(void *arg) {
  data[4]++;
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  data[4]++;
  return 0;
}
