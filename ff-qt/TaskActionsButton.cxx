#include "Builder.hxx"
#include "TaskActionsButton.hxx"


TaskActionsButton::TaskActionsButton(StorageHandle storageHandle, NoteId id) {
    auto storage = Storage{storageHandle};
    setMenu(
        New<QMenu>().addAction("Postpone", [=]{ storage.postpone(id); })
    );
    setPopupMode(InstantPopup);
    setText("⋮");
}
