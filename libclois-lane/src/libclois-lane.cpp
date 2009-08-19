// libclois-lane.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the clois-lane root directory for more info.

#include <iostream>
#include <string>

#include "OIS.h"

#if defined(_WIN32)
#include "Windows.h"
#endif

using namespace std;


// Pointers to Common Lisp functions

void (*clfun_key_pressed)(int, unsigned int);
void (*clfun_key_released)(int, unsigned int);
void (*clfun_mouse_moved)(int, int, int, int, int, int);
void (*clfun_mouse_pressed)(int);
void (*clfun_mouse_released)(int);


// Variables

//InputHandler* ois_input_handler;


// Classes

class InputHandler : OIS::KeyListener, public OIS::MouseListener
{
    private:
        OIS::InputManager* ois;
        OIS::Keyboard* keyboard;
        OIS::Mouse* mouse;

    public:
        InputHandler(std::string hWnd, bool hide_mouse);
        ~InputHandler();

        void capture();
        void setWindowExtents(int width, int height);

        // KeyListener
        bool keyPressed(const OIS::KeyEvent &evt);
        bool keyReleased(const OIS::KeyEvent &evt);

        // MouseListener
        bool mouseMoved(const OIS::MouseEvent &evt);
        bool mousePressed(const OIS::MouseEvent &evt, OIS::MouseButtonID);
        bool mouseReleased(const OIS::MouseEvent &evt, OIS::MouseButtonID);
};


// Constructors

InputHandler::InputHandler(std::string hWnd, bool hide_mouse)
{
    OIS::ParamList pl;

    pl.insert(make_pair("WINDOW", hWnd));

    // XXX: this should be settable from CL
#if defined(__linux) || defined(__unix)
    pl.insert(make_pair("XAutoRepeatOn", "false"));
    pl.insert(make_pair("x11_keyboard_grab", "false"));
    pl.insert(make_pair("x11_mouse_grab", "false"));
    if (hide_mouse) {
        pl.insert(make_pair("x11_mouse_hide", "true"));
    } else {
        pl.insert(make_pair("x11_mouse_hide", "false"));
    }
#endif

    // XXX: this should be settable from CL
#if defined(_WIN32)
    pl.insert(make_pair("w32_mouse", "DISCL_FOREGROUND" ));
    //pl.insert(make_pair("w32_mouse", "DISCL_BACKGROUND" ));
    pl.insert(make_pair("w32_mouse", "DISCL_NONEXCLUSIVE"));
    pl.insert(make_pair("w32_keyboard", "DISCL_FOREGROUND"));
    //pl.insert(make_pair("w32_keyboard", "DISCL_BACKGROUND"));
    pl.insert(make_pair("w32_keyboard", "DISCL_NONEXCLUSIVE"));
    if (hide_mouse) {
        ShowCursor(false);
    } else {
        ShowCursor(false);
    }
#endif

    ois = OIS::InputManager::createInputSystem(pl);

    keyboard = 0;
    mouse = 0;
}


// Destructors

InputHandler::~InputHandler()
{
    if (keyboard) { delete keyboard; }
    if (mouse) { delete mouse; }
    OIS::InputManager::destroyInputSystem(ois);
}


// Methods

void InputHandler::capture()
{
    if (keyboard == 0 && mouse == 0)
    {
        try {
            keyboard = static_cast<OIS::Keyboard*>
                           (ois->createInputObject(OIS::OISKeyboard, true));
            keyboard->setEventCallback(this);

            mouse = static_cast<OIS::Mouse*>
                        (ois->createInputObject(OIS::OISMouse, true));
            mouse->setEventCallback(this);

            cout << "[libclois-lane] Keyboard and mouse acquired!" << endl;
        } catch (OIS::Exception &e) {
            cout << "[libclois-lane] " << e.eText << endl;
        }
    }
    else
    {
        try {
            keyboard->capture();
            mouse->capture();
	} catch (OIS::Exception &e) {
            cout << "[libclois-lane] " << e.eText << endl;
            keyboard = 0;
            mouse = 0;
        }
    }
}


void InputHandler::setWindowExtents(int width, int height)
{
    if (mouse)  // might not be initialised yet
    {
        // Set mouse region.  If window resizes, we should alter this as well.
        const OIS::MouseState &ms = mouse->getMouseState();
        ms.height = height;
        ms.width = width;
    }
}


bool InputHandler::keyPressed(const OIS::KeyEvent &evt)
{
    clfun_key_pressed(evt.key, evt.text);
    return true;
}


bool InputHandler::keyReleased(const OIS::KeyEvent &evt)
{
    clfun_key_released(evt.key, evt.text);
    return true;
}


bool InputHandler::mouseMoved(const OIS::MouseEvent &evt)
{
    clfun_mouse_moved(evt.state.X.rel, evt.state.Y.rel, evt.state.Z.rel,
                      evt.state.X.abs, evt.state.Y.abs, evt.state.Z.abs);
    return true;
}


bool InputHandler::mousePressed(const OIS::MouseEvent &evt,
                                OIS::MouseButtonID btn)
{
    clfun_mouse_pressed(btn);
    return true;
}


bool InputHandler::mouseReleased(const OIS::MouseEvent &evt,
                                 OIS::MouseButtonID btn)
{
    clfun_mouse_released(btn);
    return true;
}


// C Wrapper Functions & Variables

InputHandler* ois_input_handler;

extern "C"
{
    // Prototypes

    void ois_capture();
    InputHandler* ois_create_input_system(const char*, bool);
    void ois_set_window_extents(int, int);


    // Functions

    void ois_capture()
    {
        ois_input_handler->capture();
    }


    InputHandler* ois_create_input_system(const char* hWnd, bool hide_mouse)
    {
        //unsigned long hWnd;
        //rw->getCustomAttribute("WINDOW", &hWnd);

        if (ois_input_handler == 0)
        {
            ois_input_handler = new InputHandler(hWnd, hide_mouse);
        }

        return ois_input_handler;
    }


    void ois_set_window_extents(int width, int height)
    {
        ois_input_handler->setWindowExtents(width, height);
    }
}
