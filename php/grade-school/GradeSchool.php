<?php

declare(strict_types=1);

class School
{
    private $students = [];

    public function add(string $name, int $grade): void
    {
        if ($this->contains($name)) return;
        array_push($this->students, ['name' => $name, 'grade' => $grade]);
    }

    public function contains(string $name): bool
    {
        return in_array($name, array_column($this->students, 'name'));
    }

    public function grade($grade)
    {
        $class = array_filter($this->students, fn($student) => $student['grade'] == $grade);
        $names = array_column($class, 'name');
        sort($names);
        return $names;
    }

    public function studentsByGradeAlphabetical(): array
    {
        $grades = array_unique(array_column($this->students, 'grade'));
        sort($grades);
        return array_reduce(
            $grades,
            fn($roster, $grade) => $roster + [$grade => $this->grade($grade)],
            []
        );
    }
}
