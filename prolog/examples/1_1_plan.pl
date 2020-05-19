% База данных фактов

% Дисциплины осеннего семестра первого курса
course(1, fall, 'Прог').
course(1, fall, 'АиСД').
course(1, fall, 'ДиМат').
course(1, fall, 'АрхЭВМ').
course(1, fall, 'МатАн').
course(1, fall, 'ЛинАл').
course(1, fall, 'ИнЯз').

% Дисциплины весеннего семестра первого курса
course(1, spring, 'Прог').
course(1, spring, 'АиСД').
course(1, spring, 'ДиМат').
course(1, spring, 'С++').
course(1, spring, 'МатАн').
course(1, spring, 'ЛинАл').
course(1, spring, 'ИнЯз').

% Дисциплины осеннего семестра второго курса
course(2, fall, 'МатАн').
course(2, fall, 'АиСД').
course(2, fall, 'ДиМат').
course(2, fall, 'ТВМС').
course(2, fall, 'С++').
course(2, fall, 'ОС').
course(2, fall, 'Web').
course(2, fall, 'Android').
course(2, fall, 'ИнЯз').
course(2, fall, 'БЖД').
course(2, fall, 'История').

% Дисциплины весеннего семестра второго курса
course(2, spring, 'МатАн').
course(2, spring, 'АиСД').
course(2, spring, 'ДиМат').
course(2, spring, 'Java').
course(2, spring, 'МатЛог').
course(2, spring, 'ОС').
'course'(2, spring, 'ИнЯз').
course(2, 'spring', 'ТВМС').

% Запросы фактов

/*
?- course(1, spring, 'Прог'). % Читается ли программирование весной первого курса.
   yes
?- course(1, spring, 'ТВМС'). % Читается ли ТВМС весной первого курса.
   no
?- course(1, spring, X). % Дисциплины весеннего семестра первого курса
   X / 'Прог'
   X / 'АиСД'
   X / 'ДиМат'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
?- course(1, _, X). % Дисциплины первого курса
   X / 'Прог'
   X / 'АиСД'
   X / 'ДиМат'
   X / 'АрхЭВМ'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
   X / 'Прог'
   X / 'АиСД'
   X / 'ДиМат'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
?- course(_, spring, X). % Дисциплины весеннего семестра
    X / 'Прог'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'С++'
    X / 'МатАн'
    X / 'ЛинАл'
    X / 'ИнЯз'
    X / 'МатАн'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'Java'
    X / 'МатЛог'
    X / 'ОС'
    X / 'ИнЯз'
    X / 'ТВМС'
?- course(Y, S, 'АиСД'). % Когда читаются АиСД
   Y / 1, S / fall
   Y / 1, S / spring
   Y / 2, S / fall
   Y / 2, S / spring
?- course(1, fall, X), course(1, spring, X). % Дисциплины читаемые в обоих семестрах первого курса
    X / 'Прог'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'МатАн'
    X / 'ЛинАл'
    X / 'ИнЯз'
?- course(1, spring, X); course(2, fall, X). % Дисцилины, читаемые во втором или третьем семестрах.
    X / 'Прог'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'С++'
    X / 'МатАн'
    X / 'ЛинАл'
    X / 'ИнЯз'
    X / 'МатАн'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'ТВМС'
    X / 'С++'
    X / 'ОС'
    X / 'Web'
    X / 'Android'
    X / 'ИнЯз'
    X / 'БЖД'
    X / 'История'
*/

% Простое правило
fall(Y, C) :- course(Y, fall, C).
/*
?- fall(X, 'МатАн').
   X / 1
   X / 2
?- fall(X, 'Прог').
   X / 1
*/


% Составное правило (конъюнкция)
repeated(C) :- course(Y1, S1, C), course(Y2, S2, C), (Y1, S1) \= (Y2, S2).
/*
?- repeated(C).
    X / 'Прог'
    X / 'АиСД'
    X / 'АиСД'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'МатАн'
    X / 'МатАн'
    X / 'МатАн'
    X / 'ЛинАл'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'Прог'
    X / 'АиСД'
    X / 'АиСД'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'С++'
    X / 'МатАн'
    X / 'МатАн'
    X / 'МатАн'
    X / 'ЛинАл'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'МатАн'
    X / 'МатАн'
    X / 'МатАн'
    X / 'АиСД'
    X / 'АиСД'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ТВМС'
    X / 'С++'
    X / 'ОС'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'МатАн'
    X / 'МатАн'
    X / 'МатАн'
    X / 'АиСД'
    X / 'АиСД'
    X / 'АиСД'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ДиМат'
    X / 'ОС'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'ИнЯз'
    X / 'ТВМС'
?- setof(X, repeated(X), XS).
    XS / ['АиСД','ДиМат','ИнЯз','ЛинАл','МатАн','ОС','Прог','С++','ТВМС']
*/

% Составное правило (дизъюнкция)
next(Y, fall, Y, spring).
next(Y, spring, Y1, fall) :- Y1 is Y + 1.
/*
?- next(2, fall, Y, S).
   Y / 2, S / spring
?- next(1, spring, Y, S).
   Y / 2, S / fall
*/

repeated_next(Y, S, C) :- course(Y, S, C), next(Y, S, NY, NS), course(NY, NS, C).
/*
?- repeated_next(Y, S, 'МатАн').
   Y / 1  S / fall
   Y / 1  S / spring
   Y / 2  S / fall
?- repeated_next(Y, S, 'Прог').
   Y / 1  S / fall
*/

% Ориентированный граф
base(BY, BS, C, DY, DS, C) :- course(BY, BS, C), course(DY, DS, C), next(BY, BS, DY, DS).
base(1, spring, 'МатАн', 2, fall, 'ТВМС').
base(1, spring, 'Прог', 2, fall, 'Web').
base(1, spring, 'Прог', 2, fall, 'МатЛог').
base(1, fall, 'АрхЭВМ', 1, spring, 'C++').
/*
?- base(1, fall, 'МатАн', Y, S, C).
   Y / 1, S / spring,  C / 'МатАн'
?- base(1, spring, 'МатАн', Y, S, C).
   Y / 2, S / fall,  C / 'МатАн'
   Y / 2, S / fall,  C / 'ТВМС'
*/

% Рекурсивный поиск достижимых вершин
base_rec(BY, BS, BC, DY, DS, DC) :- base(BY, BS, BC, DY, DS, DC).
base_rec(BY, BS, BC, DY, DS, DC) :- base(BY, BS, BC, IY, IS, IC), base_rec(IY, IS, IC, DY, DS, DC).
/*
?- base_rec(1, fall, 'МатАн', Y, S, C).
   Y / 1, S / spring, C / 'МатАн'
   Y / 2, S / fall, C / 'МатАн'
   Y / 2, S / fall, C / 'ТВМС'
   Y / 2, S / spring, C / 'МатАн'
   Y / 2, S / spring, C / 'ТВМС'
*/

% Последний семестр дисциплины (отрицание).
last(Y, S, C) :- course(Y, S, C), \+ repeated_next(NY, NS, C).
/*
?- last(Y, S, 'МатАн').
   Y / 2, S / spring
?- last(1, fall, C).
   C / 'АрхЭВМ'
*/
