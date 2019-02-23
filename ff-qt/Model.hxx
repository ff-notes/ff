#ifndef ff_qt_Model_hxx
#define ff_qt_Model_hxx


#include <memory>
#include <vector>

#include <QtCore>

#include "FFI/Cxx.hxx"


class Model: public QAbstractItemModel {
private:
    struct Impl;
    std::unique_ptr<Impl> p;

    virtual int columnCount(const QModelIndex &) const;
    virtual QVariant data(const QModelIndex &, int) const;
    virtual int rowCount(const QModelIndex &) const;

public:
    Model();

    virtual
    QModelIndex
    index(int row, int column = 0, const QModelIndex & parent = {}) const;

    virtual QModelIndex parent(const QModelIndex &) const;
    Note task(QModelIndex);
    void upsertTask(Note task);
};


#endif // ff_qt_Model_hxx
