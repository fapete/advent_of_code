#include <iostream>
#include <vector>
#include <string>
#include <numeric>

/*
 * A little warning: Much of this "library" is not correct. Matrix/Vector multiplication currently results in the transpose of the correct result.
 * Gaussian Elimination is not working correctly. Matrix should be generic and allow any numeric type.
 * 
 * This may be fixed if necessary in the future, but currently it was enough to solve the one puzzle I needed this for.
 */

namespace matrix {
    struct Matrix;
    Matrix identity(size_t);
    void gaussian_elimination(Matrix&, Matrix&);

    struct Fraction {
        long num;
        long den;

        Fraction(long num, long den): num{num}, den{den} {}

        Fraction(long num): num{num}, den{1} {}

        Fraction operator+(const Fraction& other) const {
            return Fraction{num * other.den + other.num * den, den * other.den}.reduce();
        }

        Fraction operator-(const Fraction& other) const {
            return *this + Fraction{-other.num, other.den}.reduce();
        }

        Fraction operator*(const Fraction& other) const {
            return Fraction{num * other.num, den * other.den}.reduce();
        }

        Fraction operator*(const long s) const {
            return Fraction{num * s, den}.reduce();
        }

        Fraction operator/(const Fraction& other) const {
            return Fraction{num * other.den, den * other.num}.reduce();
        }

        Fraction operator/(const long s) const {
            return Fraction{num, den * s}.reduce();
        }

        Fraction operator+=(const Fraction& other) {
            *this = *this + other;
            return this->reduce();
        }

        Fraction operator*=(const Fraction& other) {
            *this = *this * other;
            return this->reduce();
        }

        Fraction operator-=(const Fraction& other) {
            *this = *this - other;
            return this->reduce();
        }

        Fraction operator/=(const Fraction& other) {
            *this = *this / other;
            return this->reduce();
        }

        bool operator==(const Fraction& other) const {
            return num == other.num && den == other.den;
        }

        bool operator!=(const Fraction& other) const {
            return !(*this == other);
        }

        long get() const {
            return num / den;
        }

    private:
        Fraction reduce() {
            long gcd = std::gcd(num, den);
            num /= gcd;
            den /= gcd;
            return *this;
        }

        friend std::ostream& operator<<(std::ostream& os, const Fraction& f);
    };

    std::ostream& operator<<(std::ostream& os, const Fraction& f) {
        os << f.num << "/" << f.den << "(" << f.get() << ")";
        return os;
    }

    struct Matrix {
        struct MatrixArithmeticError: public std::exception {
            std::string msg;

            MatrixArithmeticError(std::string message): msg{message} {}
                

            const char* what() const noexcept override {
                return msg.c_str();
            }
        };

        std::vector<std::vector<Fraction>> m;

        Matrix(std::vector<std::vector<Fraction>> m): m{m} {}
        
        size_t dim_x() const {
            return m[0].size();
        }

        size_t dim_y() const {
            return m.size();
        }

        Fraction& operator()(size_t x, size_t y) {
            return m[y][x];
        }

        const Fraction& operator()(size_t x, size_t y) const {
            return m[y][x];
        }

        Matrix operator+(const Matrix& other) const {
            if (other.dim_x() != dim_x() || other.dim_y() != dim_y()) {
                throw MatrixArithmeticError{"Matrix dimensions do not match"};
            }

            std::vector<std::vector<Fraction>> new_m;
            for (size_t y = 0; y < dim_y(); y++) {
                std::vector<Fraction> row;
                for (size_t x = 0; x < dim_x(); x++) {
                    row.push_back((*this)(x, y) + other(x, y));
                }
                new_m.push_back(row);
            }
            return Matrix{new_m};
        }

        Matrix operator-(const Matrix& other) const {
            return *this + (other * Fraction{-1, 1});
        }

        Matrix operator*(const Matrix& other) const {
            if (dim_x() != other.dim_y()) {
                throw MatrixArithmeticError{"Matrix dimensions do not match"};
            }

            std::vector<std::vector<Fraction>> new_m;
            for (size_t x = 0; x < other.dim_x(); x++) {
                std::vector<Fraction> row;
                for (size_t y = 0; y < dim_y(); y++) {
                    Fraction sum{0,1};
                    for (size_t i = 0; i < dim_x(); i++) {
                        sum = sum + (*this)(i, y) * other(x, i);
                    }
                    row.push_back(sum);
                }
                new_m.push_back(row);
            }
            return Matrix{new_m};
        }

        Matrix operator*(const Fraction s) const {
            std::vector<std::vector<Fraction>> new_m;
            for (size_t y = 0; y < dim_y(); y++) {
                std::vector<Fraction> row;
                for (size_t x = 0; x < dim_x(); x++) {
                    row.push_back((*this)(x, y) * s);
                }
                new_m.push_back(row);
            }
            return Matrix{new_m};
        }

        Matrix transpose() const {
            std::vector<std::vector<Fraction>> new_m;
            for (size_t x = 0; x < dim_x(); x++) {
                std::vector<Fraction> row;
                for (size_t y = 0; y < dim_y(); y++) {
                    row.push_back((*this)(x, y));
                }
                new_m.push_back(row);
            }
            return Matrix{new_m};
        }

        void swap_rows(size_t row1, size_t row2) {
            std::swap(m[row1], m[row2]);
        }

        void scale_row(size_t row, Fraction s) {
            if (s.get() == 0) {
                throw MatrixArithmeticError{"Cannot scale row by 0"};
            }

            for (size_t x = 0; x < dim_x(); x++) {
                (*this)(x, row) *= s;
            }
        }

        void add_row(size_t row1, size_t row2, Fraction s) {
            for (size_t x = 0; x < dim_x(); x++) {
                (*this)(x, row1) = (*this)(x, row1) + (*this)(x, row2) * s;
            }
        }

        Matrix inverse() const {
            Matrix m = *this; // Copy
            if (dim_x() == dim_y() && dim_x() == 2) {
                long det = m(0, 0).get() * m(1, 1).get() - m(0, 1).get() * m(1, 0).get();
                if (det == 0) {
                    throw MatrixArithmeticError{"Matrix is singular"};
                }

                std::swap(m(0, 0), m(1, 1));
                m(0, 1) *= Fraction{-1,1};
                m(1, 0) *= Fraction{-1,1};
                m = m * Fraction{1, det};
                return m;
            }

            Matrix id = identity(dim_x());
            gaussian_elimination(m, id);
            return id;
        }

        std::ostream& operator<<(std::ostream& os) const {
            for (size_t y = 0; y < dim_y(); y++) {
                for (size_t x = 0; x < dim_x(); x++) {
                    os << m[y][x] << " ";
                }
                os << std::endl;
            }
            return os;
        }

        bool operator==(const Matrix& other) const {
            if (dim_x() != other.dim_x() || dim_y() != other.dim_y()) {
                return false;
            }

            for (size_t x = 0; x < dim_x(); x++) {
                for (size_t y = 0; y < dim_y(); y++) {
                    if ((*this)(x, y) != other(x, y)) {
                        return false;
                    }
                }
            }

            return true;
        }

        bool operator!=(const Matrix& other) const {
            return !(*this == other);
        }

        friend std::ostream& operator<<(std::ostream& os, const Matrix& m);
    };

    std::ostream& operator<<(std::ostream& os, const Matrix& m) {
        return m.operator<<(os);
    }

    Matrix identity(size_t size) {
        std::vector<std::vector<Fraction>> m;
        for (size_t y = 0; y < size; y++) {
        std::vector<Fraction> row;
            for (size_t x = 0; x < size; x++) {
                row.push_back(x == y ? 1 : 0);
            }
            m.push_back(row);
        }
        return Matrix{m};
    }

    void gaussian_elimination(Matrix& m, Matrix& target) {
        if (m.dim_x() != m.dim_y()) {
            throw Matrix::MatrixArithmeticError{"Matrix is not square"};
        }

        for (size_t i = 0; i < m.dim_x(); i++) {
            size_t max_row = i;
            for (size_t j = i + 1; j < m.dim_y(); j++) {
                if (std::abs(m(i, j).get()) > std::abs(m(i, max_row).get())) {
                    max_row = j;
                }
            }
            m.swap_rows(i, max_row);
            target.swap_rows(i, max_row);

            if (m(i, i) == 0) {
                throw Matrix::MatrixArithmeticError{"Matrix is singular"};
            }

            m.scale_row(i, Fraction{1} / m(i, i));
            target.scale_row(i, Fraction{1} / m(i, i));

            for (size_t j = 0; j < m.dim_y(); j++) {
                if (j != i) {
                    m.add_row(j, i, Fraction{-1} * m(i, j));
                    target.add_row(j, i, Fraction{-1} * m(i, j));
                }
            }
        }
    }
}