import numpy as np
import warnings

def my_rmse(y1, y2):
    """
    Вычисляет RMSE между y1 и y2.
    """
    y1, y2 = np.array(y1), np.array(y2)
    return np.sqrt(np.sum((y1 - y2) ** 2) / len(y1))

def cosine_sim(y1, y2):
    """
    Вычисляет косинусное сходство между y1 и y2.
    """
    y1, y2 = np.array(y1), np.array(y2)
    norm1 = np.sqrt(np.sum(y1 ** 2))
    norm2 = np.sqrt(np.sum(y2 ** 2))
    if norm1 == 0 or norm2 == 0:
        return 0
    return np.sum(y1 * y2) / (norm1 * norm2)

def cosine_dist(y1, y2):
    """
    Вычисляет косинусное расстояние:
    sqrt(2 * (1 - cosine_sim)).
    """
    return np.sqrt(2 * (1 - cosine_sim(y1, y2)))

def get_max_indx(row):
    """
    Возвращает индексы (1-индексация) трёх наибольших элементов в row в порядке убывания.
    """
    row = np.array(row)
    # np.argsort возвращает индексы по возрастанию; для убывания сортируем по -row
    return np.argsort(-row)[:3] + 1  # добавляем 1 для перехода к 1-индексации

def calc_pair_match(x, y):
    """
    Вычисляет оценку совпадения двух позиций x и y.
    x и y — целые числа от 1 до 6.
    
    Правила:
      - Если x == y, то 3.
      - Если x == ((y - 2) % 6 + 1) или x == (y % 6 + 1), то 2.
      - Если x == ((y - 3) % 6 + 1) или x == ((y + 1) % 6 + 1), то 1.
      - Если x == ((y + 2) % 6 + 1), то 0.
    """
    if x == y:
        return 3
    if x == (((y - 2) % 6) + 1) or x == ((y % 6) + 1):
        return 2
    if x == (((y - 3) % 6) + 1) or x == (((y + 1) % 6) + 1):
        return 1
    if x == (((y + 2) % 6) + 1):
        return 0
    return 0  # на случай, если ни одно условие не сработало

def calc_C_index(row1, row2):
    """
    Вычисляет C-индекс между двумя векторами.
    C = 3 * match(1) + 2 * match(2) + 1 * match(3),
    где match(i) вычисляется для i-ой позиции в топ-3 наибольших значениях.
    """
    r1 = get_max_indx(row1)
    r2 = get_max_indx(row2)
    return (calc_pair_match(r1[0], r2[0]) * 3 +
            calc_pair_match(r1[1], r2[1]) * 2 +
            calc_pair_match(r1[2], r2[2]) * 1)

# Функция расстояния по C-индексу: чем больше C-индекс, тем лучше предсказание, поэтому
# чтобы минимальное значение соответствовало лучшему предсказанию, определяем:
c_index_dist = lambda row1, row2: 18 - calc_C_index(row1, row2)

def smart_integer_round(six_vals):
    """
    Функция округления вектора из 6 значений так, чтобы их сумма была равна 42.
    Алгоритм:
      1. Масштабирует значения так, чтобы сумма стала 42 (с добавлением небольшого eps для стабильности).
      2. Вычисляет остаток (resid) между 42 и суммой округлённых значений.
      3. Корректирует те элементы, у которых разница между исходным значением и округленным максимальна,
         в зависимости от знака resid.
    """
    six_vals = np.array(six_vals, dtype=float)
    epsilon = 10 * np.finfo(float).eps
    modif = six_vals * 42 / np.sum(six_vals) + epsilon
    resid = int(42 - np.sum(np.round(modif)))
    
    differences = np.abs(modif - np.round(modif))
    # Сортируем индексы по убыванию разницы
    ind_sorted = np.argsort(-differences)
    n_adjust = abs(resid)
    indices_to_change = ind_sorted[:n_adjust]
    
    modif[indices_to_change] += np.sign(resid) * 1
    
    int_rnd_values = np.round(modif).astype(int)
    if np.sum(int_rnd_values) != 42:
        warnings.warn(f"Sum of {int_rnd_values} != 42. Input: {six_vals}.")
    return int_rnd_values

def prediction_correction(preds):
    """
    Применяет smart_integer_round к каждой строке массива предсказаний.
    preds должен быть 2D-массивом, где каждая строка содержит 6 значений.
    """
    preds = np.array(preds)
    corrected = np.apply_along_axis(smart_integer_round, 1, preds)
    return corrected

def df_metric(pred_df, Y_test_, func=my_rmse):
    """
    Вычисляет метрику (по умолчанию RMSE) для каждой строки между pred_df и Y_test_,
    а затем возвращает среднее значение метрики.
    pred_df и Y_test_ должны быть массивами numpy.
    """
    pred_df = np.array(pred_df)
    Y_test_ = np.array(Y_test_)
    metrics = [func(pred_df[i, :], Y_test_[i, :]) for i in range(pred_df.shape[0])]
    return np.mean(metrics)

## Пример использования функций:
# if __name__ == "__main__":
#     # Пример для RMSE, косинусного сходства и расстояния
#     y1 = np.array([1, 2, 3, 4, 5, 6])
#     y2 = np.array([6, 5, 4, 3, 2, 1])
#     print("RMSE:", my_rmse(y1, y2))
#     print("Cosine similarity:", cosine_sim(y1, y2))
#     print("Cosine distance:", cosine_dist(y1, y2))
    
#     # Пример для C-индекса
#     print("C-index:", calc_C_index(y1, y2))
#     print("C-index distance:", c_index_dist(y1, y2))
    
#     # Пример для smart_integer_round
#     six_vals = [10, 10, 10, 10, 1, 1]
#     print("Smart integer round:", smart_integer_round(six_vals))
    

    
#     # Пример для df_metric
#     pred_df = np.array([[1, 2, 3, 4, 5, 6],
#                         [6, 5, 4, 3, 2, 1]])
#     Y_test_ = np.array([[1, 2, 3, 4, 5, 6],
#                         [6, 5, 4, 3, 2, 1]])
#     print("df_metric (RMSE):", df_metric(pred_df, Y_test_))