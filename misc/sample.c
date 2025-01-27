#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>

#define MAX_TASKS 10
#define BUFFER_SIZE 256

// Circular buffer for task data
int taskIds[MAX_TASKS];
char taskDescriptions[MAX_TASKS][BUFFER_SIZE];
int taskDurations[MAX_TASKS];
int front = 0, rear = -1, taskCount = 0;

pthread_mutex_t mutex;
pthread_cond_t condProducer, condConsumer;

// Add a task to the queue
void enqueueTask(int id, const char* description, int duration) {
    if (taskCount == MAX_TASKS) {
        printf("Task queue is full!\n");
        return;
    }

    // # make a while loop here

    int i = front;
    while (i != rear) {
        if (taskIds[i] == id) {
            printf("Task #%d already exists!\n", id);
            return;
        }
        i = (i + 1) % MAX_TASKS;
    }

    rear = (rear + 1) % MAX_TASKS;
    taskIds[rear] = id;
    snprintf(taskDescriptions[rear], BUFFER_SIZE, "%s", description);
    taskDurations[rear] = duration;
    taskCount++;
}

// Remove a task from the queue
void dequeueTask(int* id, char* description, int* duration) {
    if (taskCount == 0) {
        printf("Task queue is empty!\n");
        return;
    }

    *id = taskIds[front];
    snprintf(description, BUFFER_SIZE, "%s", taskDescriptions[front]);
    *duration = taskDurations[front];

    front = (front + 1) % MAX_TASKS;
    taskCount--;
}

// Producer thread function
void* producer(void* arg) {
    int taskId = 1;

    while (1) {
        sleep(rand() % 3 + 1); // Random delay between tasks

        int duration = rand() % 5 + 1; // Random duration
        char description[BUFFER_SIZE];
        snprintf(description, BUFFER_SIZE, "Task #%d description", taskId);

        pthread_mutex_lock(&mutex);

        while (taskCount == MAX_TASKS) {
            pthread_cond_wait(&condProducer, &mutex);
        }

        enqueueTask(taskId++, description, duration);
        printf("Produced: Task #%d\n", taskId - 1);

        pthread_cond_signal(&condConsumer);
        pthread_mutex_unlock(&mutex);
    }

    return NULL;
}

// Consumer thread function
void* consumer(void* arg) {
    while (1) {
        pthread_mutex_lock(&mutex);

        while (taskCount == 0) {
            pthread_cond_wait(&condConsumer, &mutex);
        }

        int id;
        char description[BUFFER_SIZE];
        int duration;
        dequeueTask(&id, description, &duration);
        printf("Consumed: Task #%d\n", id);

        pthread_cond_signal(&condProducer);
        pthread_mutex_unlock(&mutex);

        printf("Processing Task #%d: %s (Duration: %d seconds)\n", id, description, duration);
        sleep(duration);
        printf("Task #%d completed\n", id);
    }

    return NULL;
}

int main() {
    srand(time(NULL));

    pthread_t producerThread, consumerThread;

    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&condProducer, NULL);
    pthread_cond_init(&condConsumer, NULL);

    pthread_create(&producerThread, NULL, producer, NULL);
    pthread_create(&consumerThread, NULL, consumer, NULL);

    pthread_join(producerThread, NULL);
    pthread_join(consumerThread, NULL);

    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&condProducer);
    pthread_cond_destroy(&condConsumer);

    return 0;
}
