int f(int v) { return v*v; }

int sum(int* list, int length) {
    int sum = 0;
    for (int i = 0; i < length; ++i)
        sum += f(list[i]);
    return sum;
}

int arr[5] = {1,2,3,4,5};
printf("%d\n", sum(arr, 5));
