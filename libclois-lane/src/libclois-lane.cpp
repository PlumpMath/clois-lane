// libclois-lane.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the clois-lane root directory for more info.

#include <iostream>
#include <string>

#include "OIS.h"

using namespace std;


// Pointers to Common Lisp functions

void (*clfun_key_pressed)(int);
void (*clfun_key_released)(int);
void (*clfun_mouse_moved)(int, int);
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
        InputHandler(std::string hWnd);
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

InputHandler::InputHandler(std::string hWnd)
{
    OIS::ParamList pl;

    pl.insert(make_pair("WINDOW", hWnd));

    // XXX: this should be settable from CL
#if defined(__linux) || defined(__unix)
    pl.insert(make_pair("XAutoRepeatOn", "false"));
    pl.insert(make_pair("x11_keyboard_grab", "false"));
    pl.insert(make_pair("x11_mouse_grab", "false"));
    pl.insert(make_pair("x11_mouse_hide", "false"));
#endif

    // XXX: this should be settable from CL
#if defined(_WIN32)
    pl.insert(make_pair("w32_mouse", "DISCL_FOREGROUND" ));
    //pl.insert(make_pair("w32_mouse", "DISCL_BACKGROUND" ));
    pl.insert(make_pair("w32_mouse", "DISCL_NONEXCLUSIVE"));
    pl.insert(make_pair("w32_keyboard", "DISCL_FOREGROUND"));
    //pl.insert(make_pair("w32_keyboard", "DISCL_BACKGROUND"));
    pl.insert(make_pair("w32_keyboard", "DISCL_NONEXCLUSIVE"));
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

            cout << "[liblois-lane] Keyboard and mouse acquired!" << endl;
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
    clfun_key_pressed(evt.key);
    return true;
}


bool InputHandler::keyReleased(const OIS::KeyEvent &evt)
{
    clfun_key_released(evt.key);
    return true;
}


bool InputHandler::mouseMoved(const OIS::MouseEvent &evt)
{
    // This is incomplete and only works for the x and y axes.
    // Note: the scrollwheel and buttons beyond the fifth also count
    // as additional axes.
    clfun_mouse_moved(evt.state.X.rel, evt.state.Y.rel);
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
    InputHandler* ois_create_input_system(const char*);
    void ois_set_window_extents(int, int);


    // Functions

    void ois_capture()
    {
        ois_input_handler->capture();
    }


    InputHandler* ois_create_input_system(const char* hWnd)
    {
        //unsigned long hWnd;
        //rw->getCustomAttribute("WINDOW", &hWnd);

        if (ois_input_handler == 0)
        {
            ois_input_handler = new InputHandler(hWnd);
        }

        return ois_input_handler;
    }


    void ois_set_window_extents(int width, int height)
    {
        ois_input_handler->setWindowExtents(width, height);
    }
}