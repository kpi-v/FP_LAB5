# FP_LAB5

<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
</p>
<p align="right">Студент: Валентьєв В.В. КВ-21<p>
<p align="right">Рік: 2025 (дод. сесія)<p>

## Завдання

 Реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею. 
1.  Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом). 
2.  Розробити утиліту(-и) для зчитування таблиць з файлів. 
3.  Розробити функцію  select	, яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або 
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька.  select	 повертає лямбда-вираз, 
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у  select	. При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів. 
4.  Написати утиліту(-и) для запису вибірки (списку записів) у файл. 
5.  Написати функції для конвертування записів у інший тип (в залежності від варіанту): 
структури у геш-таблиці 
геш-таблиці у асоціативні списки 
асоціативні списки у геш-таблиці 
6.  Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 2
База даних: Виробництво дронів

Тип запису: Геш-таблиця

Таблиці: 
1. Виробники дронів;
2. Дрони
   
Опис: База даних виробників дронів та, власне, дронів. 


## Лістинг реалізації завдання
```lisp
;-----------------------------------------------------
; adittional functions
;-----------------------------------------------------

(defun split-string (string delimiter)
  (let ((result '())
        (current '()))
    (loop for char across string
          do (if (char= char delimiter)
                 (progn
                   (push (coerce-to-string current) result)
                   (setf current '()))
                 (push char current)))
    (unless (null current)
      (push (coerce-to-string current) result))
    (reverse result)))

(defun coerce-to-string (char-list)
  (if char-list
      (coerce (nreverse char-list) 'string)
      ""))

;-----------------------------------------------------
; main functions
;-----------------------------------------------------

(defun read-csv-to-hash (file-path)
  (with-open-file (stream file-path :direction :input)
    (let* ((header (split-string (read-line stream) #\,))
           (records '()))
      (loop for line = (read-line stream nil nil)
            while line do
              (let* ((values (split-string line #\,))
                     (record (make-hash-table :test 'equal)))
                (loop for key in header
                      for value in values
                      do (setf (gethash key record) value))
                (push record records)))
      (reverse records))))

(defun select (file-path)
  (let ((records (read-csv-to-hash file-path)))
    (lambda (&key (filter nil))
      (if filter
          (remove-if-not (lambda (record)
                           (every (lambda (pair)
                                    (equal (gethash (car pair) record) (cdr pair)))
                                  filter))
                         records)
          records))))

(defun write-hashes-to-csv (records file-path)
  (when records
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (let* ((header (loop for k being the hash-keys of (first records) collect k)))
        (format stream "~{~A~^,~}~%" header)
        (dolist (record records)
          (format stream "~{~A~^,~}~%"
                  (loop for key in header
                        collect (gethash key record ""))))))))

(defun hash-to-alist (hash)
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hash)
    alist))

(defun print-records (records)
  (dolist (record records)
    (maphash (lambda (k v)
               (format t "~A: ~A~%" k v))
             record)))
```

## Тестові набори та утиліти
```lisp
(defun test-utils ()
  (format t "--- Test ---~%")
  (let ((file "drones.csv"))
    (format t "~%Read csv to hash test: ~%")
    (let ((records (read-csv-to-hash file)))
      (print-records records)))

  (let ((file "drones.csv"))
    (format t "~%Select test: ~%")
    (let ((selector (select file)))
      (let ((all-records (funcall selector)))
        (format t "All records: ~%")
        (print-records all-records))
      (let ((filtered-records (funcall selector :filter '(("Model" . "X1")))))
        (format t "Filtred records (Model=X1): ~%")
        (print-records filtered-records))))

  (let ((output-file "output.csv"))
    (format t "~%Write to csv test: ~%")
    (let ((records (read-csv-to-hash "drones.csv")))
      (write-hashes-to-csv records output-file)
      (format t "Records saved to ~A~%" output-file)))

  (format t "~%Convert hash to align test: ~%")
  (let* ((hash (make-hash-table :test 'equal))
         (alist nil))
    (setf (gethash "Key1" hash) "Value1")
    (setf (gethash "Key2" hash) "Value2")
    (setf alist (hash-to-alist hash))
    (format t "Converting from hash table to align list: ~A~%" alist)
    (maphash (lambda (k v)
               (format t "~A: ~A~%" k v))
             hash)))
```

## Тестування

``` lisp
--- Test ---

Read csv to hash test:
manufacturer_id: 1
price: 1500
type: Quadcopter
name: Mavic 3
id: 1
manufacturer_id: 2
price: 1200
type: Quadcopter
name: Anafi
id: 2
manufacturer_id: 3
price: 3000
type: Fixed-Wing
name: X2
id: 3

Select test:
All records:
manufacturer_id: 1
price: 1500
type: Quadcopter
name: Mavic 3
id: 1
manufacturer_id: 2
price: 1200
type: Quadcopter
name: Anafi
id: 2
manufacturer_id: 3
price: 3000
type: Fixed-Wing
name: X2
id: 3
Filtred records (Model=X1):

Write to csv test:
Records saved to output.csv

Convert hash to align test:
Converting from hash table to align list: ((Key1 . Value1) (Key2 . Value2))
NIL
```
