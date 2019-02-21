#ifndef ff_qt_Model_hxx
#define ff_qt_Model_hxx


#include <vector>
#include <QtCore>

#include "FFI/Cxx.hxx"


class Model: public QAbstractItemModel {
private:
    std::vector<Note> tasks;
    std::map<std::string, size_t> taskIndex;
    virtual int columnCount(const QModelIndex &) const;
    virtual QVariant data(const QModelIndex &, int) const;
    virtual int rowCount(const QModelIndex &) const;
public:
    Model();
    virtual QModelIndex index(int, int, const QModelIndex &) const;
    virtual QModelIndex parent(const QModelIndex &) const;
    Note task(QModelIndex);
    void upsertTask(Note task);
};


#endif // ff_qt_Model_hxx
