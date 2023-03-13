void memcpy(int* dest, int* src, int n) {
    while(n) {
        *dest++ = *src++;
        n--;
    }
}

void strcpy(char* dest, char* src) {
    while(*src) {
        *dest++ = *src++;
    }
    *dest = *src; // copy '\0'
}

void prints(char * s) {
    while(*s) {
        print(*s);
        s = s + 1;
    }
}

struct person_t {
    char name[16];
    char surname[16];
    int age;
};

struct family_t {
    person_t mother;
    person_t father;
    int kid_count;
    person_t kids[3];
};

void print_person(person_t* person) {
    prints("Name: ");
    prints(person->name);
    prints("\n");

    prints("Surname: ");
    prints(person->surname);
    prints("\n");

    prints("Age: ");
    printnum(person->age);
}

void print_family(family_t* family) {
    prints("=== Family ===\n");
    prints("# Mother\n");
    print_person(&family->mother);
    prints("# Father\n");
    print_person(&family->father);
    for(int i = 0; i < family->kid_count; i++) {
        prints("# Kid #");
        printnum(i + 1);
        print_person(&family->kids[i]);
    }
}

family_t age_family(family_t f, int by) {
    f.mother.age = f.mother.age + by;
    f.father.age = f.father.age + by;
    for(int i = 0; i < f.kid_count; i++)
        f.kids[i].age = f.kids[i].age + by;
    return f;
}

int main() {
    family_t family;
    person_t mother;
    strcpy(mother.name, "Emily");
    strcpy(mother.surname, "Armstrong");
    mother.age = 35;

    person_t father;
    strcpy(father.name, "John");
    strcpy(father.surname, "Armstrong");
    father.age = 40;

    person_t kid_1;
    strcpy(kid_1.name, "Martin");
    strcpy(kid_1.surname, "Armstrong");
    kid_1.age = 7;

    person_t kid_2 = kid_1; // twins
    strcpy(kid_2.name, "Adam");

    family.mother = mother;
    family.father = father;
    family.kid_count = 2;
    family.kids[0] = kid_1;
    family.kids[1] = kid_2;

    family_t aged = age_family(family, 5);

    print_family(&family);
    prints("... after 5 years\n");
    print_family(&aged);

    return 0;
}

// > === Family ===
// > # Mother
// > Name: Emily
// > Surname: Armstrong
// > Age: 35
// > # Father
// > Name: John
// > Surname: Armstrong
// > Age: 40
// > # Kid #1
// > Name: Martin
// > Surname: Armstrong
// > Age: 7
// > # Kid #2
// > Name: Adam
// > Surname: Armstrong
// > Age: 7
// > ... after 5 years
// > === Family ===
// > # Mother
// > Name: Emily
// > Surname: Armstrong
// > Age: 40
// > # Father
// > Name: John
// > Surname: Armstrong
// > Age: 45
// > # Kid #1
// > Name: Martin
// > Surname: Armstrong
// > Age: 12
// > # Kid #2
// > Name: Adam
// > Surname: Armstrong
// > Age: 12