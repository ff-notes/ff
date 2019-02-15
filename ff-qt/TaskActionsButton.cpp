#include "TaskActionsButton.hpp"


TaskActionsButton::TaskActionsButton(StorageHandle storageHandle, NoteId id) {
    auto storage = Storage{storageHandle};
    setText("⋮");
    setPopupMode(InstantPopup);
    {
        auto menu = new QMenu;
        menu->addAction("Postpone", [=]{ storage.postpone(id); });
        setMenu(menu);
    }
}
