#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef enum { LEFT, RIGHT } direction;

typedef struct rotation {
  direction dir;
  int degrees;
} rotation;

typedef struct Node {
  struct rotation rot;
  struct Node *next;
} Node;

int has_next(Node *node) {
  printf("fine");
  return (node->next != NULL);
}

static void free_list(Node *head) {
  while (head) {
    struct Node *n = head->next;
    free(head);
    head = n;
  }
}

static void print_rot(rotation *rot) {
  const char *dir = "";

  if (rot->dir == LEFT) {
    dir = "left";
  } else {
    dir = "right";
  }

  printf("Turn %d degrees to the %s\n", rot->degrees, dir);
}
int solve_p1(Node *operations) {
  int pos = 50;
  int password = 0;
  Node *temp = operations;
  while (temp != NULL) {
    int raw = pos;
    if (temp->rot.dir == LEFT) {
      raw -= temp->rot.degrees;
    } else {
      raw += temp->rot.degrees;
    }
    int new_pos = ((raw % 100) + 100) % 100;
    pos = new_pos;
    if (new_pos == 0) {
      password++;
    }
    temp = temp->next;
  }
  return password;
}
/**
 * RAW / 100 = hits,
 *
 */
int solve_p2(Node *operations) {
  int pos = 50;
  int password = 0;

  for (Node *temp = operations; temp; temp = temp->next) {
    int d = temp->rot.degrees;
    int full = d / 100;
    int rem = d % 100;
    int threshold;

    if (temp->rot.dir == RIGHT) {
      if (pos == 0) {
        threshold = 100;
      } else {
        threshold = 100 - pos;
      }

      if (rem >= threshold) {
        full += 1;
      }

      pos = (pos + rem) % 100;
    } else { // LEFT
      if (pos == 0) {
        threshold = 100;
      } else {
        threshold = pos;
      }

      if (rem >= threshold) {
        full += 1;
      }

      pos = (pos + 100 - rem) % 100;
    }

    password += full;
    printf("rotating %d degrees, password is now: %d\n", temp->rot.degrees, password);
  }

  return password;
}

int main(int argc, char *argv[]) {
  const char *path = "src/infiles/day1.txt";
  if (argc > 1) {
    path = argv[1];
  }
  FILE *fptr = fopen(path, "r");
  if (!fptr) {
    perror("fopen");
    return 1;
  }
  Node *head = NULL, *tail = NULL, *prev = head;
  char *buf = NULL;
  size_t cap = 0;
  size_t nread;

  head = malloc(sizeof(Node));
  head->next = NULL;
  // Parse the file
  while ((nread = getline(&buf, &cap, fptr)) != -1) {
    if (nread > 0 && buf[nread - 1] == '\n') {
      buf[nread - 1] = '\0';
      nread--;
    }

    if (nread < 2) {
      continue; // need at least "L1"
    }
    Node *cur = malloc(sizeof(Node));
    char dir_char = buf[0];
    cur->rot.degrees = (int)strtol(buf + 1, NULL, 10);
    switch (dir_char) {
    case 'L':
      cur->rot.dir = LEFT;
      break;
    case 'R':
      cur->rot.dir = RIGHT;
      break;
    default:
      continue;
    }
    if (head->next == NULL) {
      head->next = cur;
    } else {
      prev->next = cur;
    }
    prev = cur;
  }

  // walk the list and do it that way
  int password = solve_p2(head);

  free(buf);
  fclose(fptr);
  free_list(head);
  printf("%d\n", password);
}
